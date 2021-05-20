package Search::Indexer;
use 5.16.0; # needed for CORE::fc
use strict;
use warnings;
use Carp;
use BerkeleyDB;
use Search::QueryParser;
use List::Util                      qw/min/;
use List::MoreUtils                 qw/uniq/;
use Text::Transliterator::Unaccent;


our $VERSION = "0.77";

use constant unaccenter => Text::Transliterator::Unaccent->new(upper => 0);
use constant {

# max size of various ids
  MAX_DOC_ID  => 0xFFFFFFFF, # unsigned long (32 bits)
  MAX_POS_ID  => 0xFFFFFFFF, # position_id

# encodings for pack/unpack
  IXDPACK     => 'ww',       # ixd values : pairs (compressed int, nb_occur)
  IXDPACK_L   => '(ww)*',    # list of above
  IXPPACK     => 'w*',       # word positions : list of compressed ints
  IXPKEYPACK  => 'ww',       # key for ixp : (docId, wordId)

  WRITECACHESIZE => (1 << 24), # arbitrary big value; seems good enough but may need tuning

# default values for args to new()
  DEFAULT     => {
    writeMode => 0,
    positions => 1,
    wregex    => qr/\p{Word}+/,
    wfilter   => sub { # default filter : lowercase and no accents
      my $word = CORE::fc($_[0]);
      unaccenter->($word);
      return $word;
    },
    fieldname => '',

    ctxtNumChars => 35,
    maxExcerpts  => 5,
    preMatch     => "<b>",
    postMatch    => "</b>",

    # constants for computing BM25 relevance. See https://fr.wikipedia.org/wiki/Okapi_BM25.
    # ElasticSearch and Sqlite FTS5 use the same values
    bm25_k1      => 1.2,
    bm25_b       => 0.75,
  }
};


sub new {
  my $class = shift;
  my $args = ref $_[0] eq 'HASH' ? $_[0] : {@_};

  # parse options
  my $self = bless {}, $class;
  $self->{$_} = exists $args->{$_} ? delete $args->{$_} : DEFAULT->{$_} 
    foreach keys %{ DEFAULT() };
  my $dir = delete $args->{dir} || ".";
  $dir =~ s{[/\\]$}{};		# remove trailing slash
  my $stopwords = delete $args->{stopwords};

  # complain if we found invalid options
  my @remaining = keys %$args;
  croak "unexpected option : $remaining[0]" if @remaining;

  # remember if this is a fresh database
  my $is_fresh_db = ! -e "$dir/ixp.bdb";

  # utility sub to connect to one of subdatabases
  my $tie_db = sub {
    my ($db_subname, $db_kind) = @_;
    my $db_file = "$dir/$db_subname.bdb";
    my @args = (-Filename => $db_file,
                (-Flags => ($self->{writeMode} ? DB_CREATE : DB_RDONLY)),
                ($self->{writeMode} ? (-Cachesize => WRITECACHESIZE) : ()));

    $self->{$db_subname . 'Db'} =
      $db_kind eq 'Recno' ? tie @{$self->{$db_subname}}, "BerkeleyDB::$db_kind", @args
                          : tie %{$self->{$db_subname}}, "BerkeleyDB::$db_kind", @args
      or croak "open $db_file : $! $^E $BerkeleyDB::Error";
  };


  # SUBDATABASE ixw : word => wordId (or -1 for stopwords)
  $tie_db->(ixw => 'Btree');

  # SUBDATABASE ixd : wordId => list of (docId, nOccur)
  $tie_db->(ixd => 'Recno');

  # SUBDATABASE ixp : (int pair) => data, namely:
  #    (0, 0)          => (total nb of docs, average word count per doc)
  #    (0, docId)      => nb of words in docId
  #    (1, 0)          => flag to store the value of $self->{positions}
  #    (docId, wordId) => list of positions of word in doc -- only if $self->{positions}
  $tie_db->(ixp => 'Btree');

  # an optional list of stopwords may be given as a list or as a filename
  if ($stopwords) {
    $self->{writeMode} or croak "must be in writeMode to specify stopwords";

    # if scalar, this is treated as the name of the stopwords file => read it
    if (not ref $stopwords) { 
      my $fh;
      open $fh, "<", $stopwords or
	open $fh, "<", "$dir/$stopwords" or
	  croak "can't open stopwords file $stopwords : $^E ";
      local $/ = undef;
      my $buf = <$fh>;
      close $buf;
      $stopwords = [$buf =~ /$self->{wregex}/g];
    }

    # store stopwords in ixw with wordId=-1
    foreach my $word (@$stopwords) {
      $self->{ixw}{$word} = -1;
    }
  }

  if ($is_fresh_db) {
    if ($self->{positions}) {
      # store an empty value under ixp(1, 0) to mark that we are using positions
      $self->ixp(1, 0) = "";
    }

    # number of words indexed in this database
    $self->{ixw}{_NWORDS} = 0;
  }


  else {
    # must know if this DB used word positions or not when it was created
    my $has_positions = defined $self->ixp(1, 0);

    croak "can't require 'positions => 1' after index creation time"
      if $self->{writeMode} && $self->{positions} && !$has_positions;

    $self->{positions} = $has_positions;
  }

  return $self;
}


sub ixp : lvalue { # utility for reading or writing into {ixp}, with pairs of ints as keys
  my ($self, $v1, $v2) = @_;
  my $ixpKey = pack IXPKEYPACK, $v1, $v2;
  return $self->{ixp}{$ixpKey};
}



sub add {
  my $self = shift;
  my $docId = shift;
  # my $buf = shift; # using $_[0] instead for efficiency reasons

  # check that this is a valid docId
  croak "docId $docId is too large"
    if $docId > MAX_DOC_ID;

  # extract words from the $_[0] buffer
  my %positions;
  my $word_position = 1;
  my $nb_words_ini = my $nb_words_fin = $self->{ixw}{_NWORDS};
 WORD:
  while ($_[0] =~ /$self->{wregex}/g) {
    my $word = $self->{wfilter} ? $self->{wfilter}->($&) : $&
      or next WORD;
    my $wordId = $self->{ixw}{$word} ||= ++$nb_words_fin;
    push @{$positions{$wordId}}, $word_position if $wordId > 0;
    $word_position += 1;
  }
  $self->{ixw}{_NWORDS} = $nb_words_fin if $nb_words_fin > $nb_words_ini;

  # record nb of words in this document -- under pair (0, $docId) in ixp
  croak "docId $docId is already used" if defined $self->ixp(0, $docId);
  $self->ixp(0, $docId) = $word_position;

  # open a cursor for more efficient write into ixd
  my $c = $self->{ixdDb}->db_cursor;

  # insert words into the indices
  foreach my $wordId (keys %positions) { 

    # insert this doc into the ixd list for $wordId
    my $n_occur     = @{$positions{$wordId}};
    my $to_be_added = pack(IXDPACK, $docId, $n_occur);
    my ($k, $v) = ($wordId, undef);
    my $status = $c->c_get($k, $v, DB_SET);
    if ($status == 0) {
      # this $wordId already has a list of docs. Let's append to that list
      my $current_length = length($v);
      $c->partial_set($current_length, length($to_be_added)); # next put() will be at end of string
      $status = $c->c_put($k, $to_be_added, DB_CURRENT);
      warn "add() : c_put error: $status\n" if $status > 0;
      $c->partial_clear;
    }
    else {
      # create a new entry for this $wordId
      $status = $self->{ixdDb}->db_put($k, $to_be_added);
      warn "add() : db_put error: $status\n" if $status > 0;
    }

    # insert the word positions into ixp
    if ($self->{positions}) {
      $self->ixp($docId, $wordId) = pack(IXPPACK, @{$positions{$wordId}});
    }
  }
}



sub remove {
  my $self = shift;
  my $docId = shift;
  # my $buf = shift; # using $_[0] instead for efficiency reasons

  # retrieve or recompute the word ids
  my $wordIds;
  if ($self->{positions}) { # if using word positions
    not defined $_[0] or carp "remove() : unexpected 'buf' argument";
    $wordIds= $self->wordIds_in_doc($docId);
  }
  else {              # otherwise : recompute word ids
    defined $_[0] or carp "remove() : need a 'buf' argument";
    $wordIds = [grep {defined $_ and $_ > 0} 
                map {$self->{ixw}{$_}}
                uniq map {$self->{wfilter} ? $self->{wfilter}->($_) : $_}
                         ($_[0] =~ /$self->{wregex}/g)];
  }

  # remove from the document index and positions index
  foreach my $wordId (@$wordIds) {
    my %docs = unpack IXDPACK_L, $self->{ixd}[$wordId];
    delete $docs{$docId};
    $self->{ixd}[$wordId] = pack IXDPACK_L, %docs;
    if ($self->{positions}) {
      my $k = pack IXPKEYPACK, $docId, $wordId;
      delete $self->{ixp}{$k};
    }
  }

  # remove nb of words from $self->{ixp}
  my $k = pack IXPKEYPACK, 0, $docId;
  delete $self->{ixp}{$k};
}

sub wordIds_in_doc {
  my $self         = shift;
  my $target_docId = shift;

  $self->{positions}
    or croak "wordIds() not available (index was created with positions=>0)";

  # position cursor at pair ($target_docId, 1n)
  my @wordIds = ();
  my $c = $self->{ixpDb}->db_cursor;
  my ($k, $v);
  $k = pack IXPKEYPACK, $target_docId, 1;
  my $status = $c->c_get($k, $v, DB_SET_RANGE);

  # proceed sequentially through the pairs with same $target_docId
  while ($status == 0) {
    my ($docId, $wordId) = unpack IXPKEYPACK, $k;
    last if $docId != $target_docId;
    push @wordIds, $wordId;
    $status = $c->c_get($k, $v, DB_NEXT);
  }

  return \@wordIds;
}



sub words {
  my $self = shift;
  my $prefix = shift;

  my $regex = qr/^$prefix/;
  my @words = ();

  # position cursor at the first word starting with the $prefix
  my $c = $self->{ixwDb}->db_cursor;
  my ($k, $v);
  $k = $prefix;
  my $status = $c->c_get($k, $v, DB_SET_RANGE);

  # proceed sequentially through the words with same $prefix
  while ($status == 0) {
    last if $k !~ $regex;
    push @words, $k;
    $status = $c->c_get($k, $v, DB_NEXT);
  }

  return \@words;
}

sub dump {
  my $self = shift;
  foreach my $word (sort keys %{$self->{ixw}}) {
    my $wordId = $self->{ixw}{$word};
    if ($wordId == -1) {
      print "$word : STOPWORD\n";
    }
    else {
      my %docs = unpack IXDPACK_L, $self->{ixd}[$wordId];
      print "$word : ", join (" ", keys %docs), "\n";
    }
  }
}

sub search {
  my $self = shift;
  my $query_string = shift;
  my $implicitPlus = shift;

  # parse the query string
  $self->{qp} ||= new Search::QueryParser;
  my $q = $self->{qp}->parse($query_string, $implicitPlus);

  # translate the query structure
  my ($translated_query, $killedWords, $wordsRegexes) = $self->translateQuery($q);

  # regex that will be used for highlighting excerpts
  my $strRegex = "(?:" . join("|", uniq @$wordsRegexes) . ")";

  # return structure
  return { scores      => $self->_search($translated_query),
	   killedWords => [keys %$killedWords],
	   regex       => qr/$strRegex/i,        };
}

sub _search {
  my ($self, $q) = @_;

  my $scores = undef;		# hash {doc1 => score1, doc2 => score2 ...}

  # 1) deal with mandatory subqueries

  foreach my $subQ ( @{$q->{'+'}} ) {
    my $sc =  $self->docsAndScores($subQ) or next;
    $scores = $sc and next if not $scores;  # if first result set, just store

    # otherwise, intersect with previous result set
    foreach my $docId (keys %$scores) {
      if (defined $sc->{$docId}) {
        $scores->{$docId} += $sc->{$docId};
      }
      else {
        delete $scores->{$docId};
      }
    }
  }

  my $noMandatorySubq = not $scores;

  # 2) deal with non-mandatory subqueries 

  foreach my $subQ (@{$q->{''}}) {
    my $sc =  $self->docsAndScores($subQ) or next;
    $scores = $sc and next if not $scores;  # if first result set, just store

    # otherwise, combine with previous result set
    foreach my $docId (keys %$sc) {
      if (defined $scores->{$docId}) { # docId was already there, add new score
	$scores->{$docId} += $sc->{$docId};
      }
      elsif ($noMandatorySubq){ # insert a new docId to the result set
	$scores->{$docId} = $sc->{$docId};
      }
      # else do nothing (ignore this docId)
    }
  }

  return undef if not $scores or not %$scores; # no results

  # 3) deal with negative subqueries (remove corresponding docs from results)

  foreach my $subQ (@{$q->{'-'}}) {
    my $negScores =  $self->docsAndScores($subQ) or next;
    delete $scores->{$_}  foreach keys %$negScores;
  }

  return $scores;
}

sub docsAndScores { # returns a hash {docId => score} or undef (no info)
  my ($self, $subQ) = @_;

  # recursive call to _search if $subQ is a parenthesized query
  return $self->_search($subQ->{value}) if $subQ->{op} eq '()';

  # otherwise, don't care about $subQ->{op} (assert $subQ->{op} eq ':')

  if (ref $subQ->{value}) { # several words, this is an "exact phrase"
    return $self->matchExactPhrase($subQ);
  }
  elsif ($subQ->{value} <= -1) {# this is a stopword
    return undef;
  }
  else { # scalar value, match single word
    # retrieve a hash, initially of shape (docId => nb_of_occurrences_of_that_word)
    my $scores = {unpack IXDPACK_L, ($self->{ixd}[$subQ->{value}] || "")};
    my @docIds = keys %$scores;
    my $n_docs_including_word = @docIds;

    # compute the bm25 relevancy score for each doc -- see https://en.wikipedia.org/wiki/Okapi_BM25
    # results are stored in same hash (overwrite the nb of occurrences)
    if ($n_docs_including_word) {
      my ($n_total_docs, $average_doc_length) = $self->global_doc_stats;

      my $inverse_doc_freq = log(($n_total_docs - $n_docs_including_word + 0.5)
                                 /
                                 ($n_docs_including_word + 0.5));

      foreach my $docId (@docIds) {
        my $freq_word_in_doc = $scores->{$docId};
        my $n_words_in_doc   = $self->ixp(0, $docId);
        my $n_words_ratio    = $n_words_in_doc / $average_doc_length;
       $scores->{$docId} = $inverse_doc_freq
                          * ( $freq_word_in_doc * ($self->{bm25_k1} + 1))
                          / ($freq_word_in_doc + $self->{bm25_k1} *
                                                (1 - $self->{bm25_b}
                                                   + $self->{bm25_b} * $n_words_ratio));
      }
    }

    return $scores;
  }
}



sub global_doc_stats {
  my ($self) = @_;

  # TODO : this should be cached until next add()

  my $n_docs      = 0;
  my $n_tot_words = 0;

  # start a cursor at pair (0, 0)
  my $c = $self->{ixpDb}->db_cursor;
  my ($k, $v);
  $k = pack IXPKEYPACK, 0, 0;
  my $status = $c->c_get($k, $v, DB_SET_RANGE);

  # proceed sequentially through the pairs of shape (0, n)
  while ($status == 0) {
    my ($RESERVED, $docId) = unpack IXPKEYPACK, $k;
    last if $RESERVED != 0;
    $n_docs      += 1;
    $n_tot_words += $v;
    $status = $c->c_get($k, $v, DB_NEXT);
  }

  my $avg = $n_docs ? $n_tot_words / $n_docs : 0;

  return ($n_docs, $avg);
}



sub matchExactPhrase {
  my ($self, $subQ) = @_;

  if (! $self->{positions}) {
    # translate into an AND query
    my $fake_query = {'+' => [map {{op    => ':',
                                    value => $_  }} @{$subQ->{value}}]};
    # and search for that one
    return $self->_search($fake_query);
  };

  # otherwise, intersect word position sets
  my %pos;
  my $wordDelta = 0;
  my $scores = undef;
  foreach my $wordId (@{$subQ->{value}}) {
    my $sc = $self->docsAndScores({op=>':', value=>$wordId});
    if (not $scores) {          # no previous result set
      if ($sc) {
        $scores = $sc;
        foreach my $docId (keys %$scores) {
          $pos{$docId} = [unpack IXPPACK, $self->ixp($docId, $wordId)];
        }
      }
    }
    else {                    # combine with previous result set
      $wordDelta++;
      foreach my $docId (keys %$scores) {
        if ($sc) { # if we have info about current word (is not a stopword)
          if (not defined $sc->{$docId}) { # current word not in current doc
            delete $scores->{$docId};
          }
          else { # current word found in current doc, check if positions match
            my @newPos   = unpack IXPPACK, $self->ixp($docId, $wordId);
            $pos{$docId} = nearPositions($pos{$docId}, \@newPos, $wordDelta);
            if ($pos{$docId}) {
              $scores->{$docId} += $sc->{$docId};
            }
            else {
              delete $scores->{$docId};
            }
          }
        }
      }  # end foreach my $docId (keys %$scores)
    }
  } # end foreach my $wordId (@{$subQ->{value}})

  return $scores;
}

sub nearPositions {
  my ($set1, $set2, $wordDelta) = @_;
# returns the set of positions in $set2 which are "close enough" (<= $wordDelta)
# to positions in $set1. Assumption : input sets are sorted.


  my @result;
  my ($i1, $i2) = (0, 0); # indices into sets

  while ($i1 < @$set1 and $i2 < @$set2) {
    my $delta = $set2->[$i2] - $set1->[$i1];
    ++$i1 and next             if $delta > $wordDelta;
    push @result, $set2->[$i2] if $delta > 0;
    ++$i2;
  }

  return @result ? \@result : undef;
}

sub translateQuery { # replace words by ids, remove irrelevant subqueries
  my ($self, $query) = @_;

  my %killedWords;
  my @wordsRegexes;

  my $recursive_translate;
  $recursive_translate = sub {
    my $q = shift;
    my $result = {};

    foreach my $k ('+', '-', '') {
      foreach my $subQ (@{$q->{$k}}) {

        # ignore items concerning other field names
        next if $subQ->{field} and $subQ->{field} ne $self->{fieldname};

        my $val = $subQ->{value};

        my $clone = undef;
        if ($subQ->{op} eq '()') {
  	$clone = { op => '()', 
  		   value => $recursive_translate->($val), };
        }
        elsif ($subQ->{op} eq ':') {
          # split query according to our notion of "term"
          my @words = ($val =~ /$self->{wregex}/g);

          # TODO : 1) accept '*' suffix; 2) find keys in $self->{ixw}; 3) rewrite into
          #        an 'OR' query

          my $regex1 = join "\\P{Word}+", map quotemeta, @words;
          my $regex2 = $self->{wfilter} ? join "\\P{Word}+", map quotemeta,
                                               map {$self->{wfilter}->($_)} @words
                                        : undef;
          foreach my $regex (grep {$_} $regex1, $regex2) {
            $regex = "\\b$regex" if $regex =~ /^\p{Word}/;
            $regex = "$regex\\b" if $regex =~ /\p{Word}$/;
          }
  	push @wordsRegexes, $regex1;
  	push @wordsRegexes, $regex2 unless $regex1 eq $regex2;

  	# now translate into word ids
  	foreach my $word (@words) {
  	  my $wf = $self->{wfilter} ? $self->{wfilter}->($word) : $word;
  	  my $wordId = $wf ? ($self->{ixw}{$wf} || 0) : -1;
  	  $killedWords{$word} = 1 if $wordId < 0;
  	  $word = $wordId;
  	}

  	$val = (@words>1) ? \@words :    # several words : return an array
  	       (@words>0) ? $words[0] :  # just one word : return its id
               0;                        # no word : return 0 (means "no info")

  	$clone = {op => ':', value=> $val};
        }

        push @{$result->{$k}}, $clone if $clone;
      }
    }

    return $result;
  };


  return ($recursive_translate->($query), \%killedWords, \@wordsRegexes);
}


sub excerpts {
  my $self = shift;
  # $_[0] : text buffer ; no copy for efficiency reason
  my $regex = $_[1];

  my $nc = $self->{ctxtNumChars};

  # find start and end positions of matching fragments
  my $matches = []; # array of refs to [start, end, number_of_matches]
  while ($_[0] =~ /$regex/g) {
    my ($start, $end) = ($-[0], $+[0]);
    if (@$matches and $start <= $matches->[-1][1] + $nc) {
      # merge with the last fragment if close enough
      $matches->[-1][1] = $end; # extend the end position
      $matches->[-1][2] += 1;   # increment the number of matches
    }
    else {
      push @$matches, [$start, $end, 1];
    }
  }

  foreach (@$matches) { # extend start and end positions by $self->{ctxtNumChars}
    $_->[0] = ($_->[0] < $nc) ? 0 : $_->[0] - $nc;
    $_->[1] += $nc;
  }

  my $excerpts = [];
  foreach my $match (sort {$b->[2] <=> $a->[2]} @$matches) {
    last if @$excerpts >= $self->{maxExcerpts};
    my $x = substr($_[0], $match->[0], $match->[1] - $match->[0]); # extract
    $x =~ s/$regex/$self->{preMatch}$&$self->{postMatch}/g ;       # highlight
    push @$excerpts, "...$x...";
  }
  return $excerpts;
}


1;


__END__

=head1 NAME

Search::Indexer - full-text indexer

=head1 SYNOPSIS

  use Search::Indexer;
  my $ix = new Search::Indexer(dir => $dir, writeMode => 1);
  foreach my $docId (keys %docs) {
    $ix->add($docId, $docs{$docId});
  }

  my $result = $ix->search('+word -excludedWord +"exact phrase"');
  my @docIds = keys %{$result->{scores}};
  my $killedWords = join ", ", @{$result->{killedWords}};
  print scalar(@docIds), " documents found\n", ;
  print "words $killedWords were ignored during the search\n" if $killedWords;
  foreach my $docId (@docIds) {
    my $score = $result->{scores}{$docId};
    my $excerpts = join "\n", $ix->excerpts($docs{$docId}, $result->{regex});
    print "DOCUMENT $docId, score $score:\n$excerpts\n\n";
  }

  my $result2 = $ix->search('word1 AND (word2 OR word3) AND NOT word4');

  $ix->remove($someDocId);

=head1 DESCRIPTION

This module provides support for indexing a collection of documents,
for searching the collection, and displaying the sorted results, 
together with contextual excerpts of the original document.

=head2 Documents

As far as this module is concerned, a I<document> is just a buffer of
plain text, together with a unique identifying number. The caller is
responsible for supplying unique numbers, and for converting the
original source (HTML, PDF, whatever) into plain text. Documents could
also contain more information (other fields like date, author, Dublin
Core, etc.), but this must be handled externally, in a database or any
other store. A candidate for storing metadata about documents
could be L<File::Tabular|File::Tabular>, which uses the same
query parser.

=head2 Search syntax

Searching requests may include plain terms, "exact phrases", 
'+' or '-' prefixes, boolean operators and parentheses.
See L<Search::QueryParser> for details.

=head2 Index files

The indexer uses three files in BerkeleyDB format : a) a mapping from
words to wordIds; b) a mapping from wordIds to lists of documents ; c)
a mapping from pairs (docId, wordId) to lists of positions within the
document. This third file holds detailed information and therefore is
quite big ; but it allows us to quickly retrieve "exact phrases"
(sequences of adjacent words) in the document.

=head2 Indexing steps

Indexing of a document buffer goes through the following
steps :

=over

=item *

terms are extracted, according to the I<wregex> regular expression

=item *

extracted terms are normalized or filtered out
by the I<wfilter> callback function. This function can for example
remove accented characters, perform lemmatization, suppress
irrelevant terms (such as numbers), etc.

=item *

normalized terms are eliminated if they belong to
the I<stopwords> list (list of common words to exclude from the index).

=item *

remaining terms are stored, together with the positions where they
occur in the document.

=back

=head2 Limits

All ids are stored as unsigned 32-bit integers; therefore there is 
a limit of 4294967295 to the number of documents or to the number of 
different words.

=head2 Related modules

A short comparison with other CPAN indexing modules is
given in the L</"SEE ALSO"> section.

This module depends on L<Search::QueryParser> for analyzing requests and 
on L<BerkeleyDB> for storing the indexes.

This module was designed together with L<File::Tabular>.

=head1 METHODS

=over

=item C<new(arg1 =E<gt> expr1, ...)>

Creates an indexer (either for a new index, or for
accessing an existing index). Parameters are :

=over

=item dir

Directory for index files. and possibly for the stopwords file. 
Default is current directory

=item writeMode

Give a true value if you intend to write into the index.

=item wregex 

Regex for matching a word (C<qr/\p{Word}+/> by default).
Will affect both L<add> and L<search> method.
This regex should not contain any capturing parentheses
(use non-capturing parentheses C<< (?: ... ) >> instead).

=item wfilter

Ref to a callback sub that may normalize or eliminate a word.  Will
affect both L<add> and L<search> method.  The default wfilter
translates words in lower case and translates latin1 (iso-8859-1)
accented characters into plain characters.

=item stopwords

List of words that will be marked into the index as "words to exclude".
This should usually occur when creating a new index ; but nothing prevents
you to add other stopwords later. Since stopwords are stored in the
index, they need not be specified when opening an index for searches or 
updates.

The list may be supplied either as a ref to an array of scalars, or 
as a the name of a file containing the stopwords (full pathname
or filename relative to I<dir>).


=item fieldname

Will only affect the L<search> method.
Search queries are passed to a general parser
(see L<Search::QueryParser>). 
Then, before being applied to the present indexer module, 
queries are pruned of irrelevant items.
Query items are considered relevant if they have no
associated field name, or if the associated field name is
equal to this C<fieldname>.

=back

Below are some additional parameters that only affect the
L</excerpts> method.

=over

=item ctxtNumChars

Number of characters determining the size of contextual excerpts
return by the L</excerpts> method.
A I<contextual excerpt> is a part of the document text,
containg a matched word surrounded by I<ctxtNumChars> characters 
to the left and to the right. Default is 35.


=item maxExcerpts

Maximum number of contextual excerpts to retrieve per document.
Default is 5.

=item preMatch

String to insert in contextual excerpts before a matched word.
Default is C<"E<lt>bE<gt>">.

=item postMatch

String to insert in contextual excerpts after a matched word.
Default is C<"E<lt>/bE<gt>">.


=item positions

  my $indexer = new Search::Indexer(dir       => $dir, 
                                    writeMode => 1,
                                    positions => 0);

Truth value to tell whether or not, when creating a new index,
word positions should be stored. The default is true.

If you turn it off, index files will be much smaller, indexing
will be faster, but results will be less precise, 
because the indexer can no longer find "exact phrases". 
So if you type  C<"quick fox jumped">, the query will be 
translated into C<quick AND fox AND jumped>, and therefore
will retrieve documents in which those three words are present, but
not necessarily in order.

Another consequence of C<< positions => 0 >> is that
there will be no automatic check of uniqueness of ids
when adding documents into the index.

=back

=item C<add(docId, buf)>

Add a new document to the index.
I<docId> is the unique identifier for this doc
(the caller is responsible for uniqueness).
I<buf> is a scalar containing the text representation of this doc.

=item C<remove(docId [, buf])>

Removes a document from the index.
If the index contains word positions (true by default), then
only the C<docId> is needed; however, if the index was created
without word positions, then the text representation
of the document must be given as a scalar string in the second argument
(of course this should be the same as the one that was supplied
when calling the L</add> method).


=item C<wordIds(docId)>

Returns a ref to an array of word Ids contained in the specified document
(not available if the index was created with C<< positions => 0 >>)

=item C<words(prefix)>

Returns a ref to an array of words found in the dictionary, 
starting with prefix (i.e. C<< $ix->words("foo") >> will
return "foo", "food", "fool", "footage", etc.).

=item C<dump()>

Debugging function, prints indexed words with list of associated docs.

=item C<search(queryString, implicitPlus)>

Searches the index.  See the L</SYNOPSIS> and L</DESCRIPTION> sections
above for short descriptions of query strings, or
L<Search::QueryParser> for details.  The second argument is optional ;
if true, all words without any prefix will implicitly take prefix '+'
(mandatory words).

The return value is a hash ref containing 

=over

=item scores

hash ref, where keys are docIds of matching documents, and values are
the corresponding computed scores.

=item killedWords

ref to an array of terms from the query string which were ignored
during the search (because they were filtered out or were stopwords)

=item regex

ref to a regular expression corresponding to all terms in the query
string. This will be useful if you later want to get contextual
excerpts from the found documents (see the L<excerpts> method).

=back

=item C<excerpts(buf, regex)>

Searches C<buf> for occurrences of C<regex>, 
extracts the occurences together with some context
(a number of characters to the left and to the right),
and highlights the occurences. See parameters C<ctxtNumChars>,
C<maxExcerpts>, C<preMatch>, C<postMatch> of the L</new> method.

=back

=head1 TO DO

=over

=item *

Find a proper formula for combining scores from several terms.
Current implementation is ridiculously simple-minded (just an addition).
Also study the literature to improve the scoring formula.

=item *

Handle concurrency through BerkeleyDB locks.

=item *

Maybe put all 3 index files as subDatabases in one single file.

=item *

Fine tuning of cachesize and other BerkeleyDB parameters.

=item *

Compare performances with other packages.

=item *

More functionalities : add NEAR operator and boost factors.

=back



=head1 SEE ALSO

L<Search::FreeText> is nice and compact, but
limited in functionality (no +/- prefixes, no "exact phrase" search,
no parentheses).

L<Plucene> is a Perl port of the Java I<Lucene> search engine.
Plucene has probably every feature you will ever need, but requires
quite an investment to install and learn (more than 60 classes,
dependencies on lots of external modules). 
I haven't done any benchmarks yet to compare performance.

L<KinoSearch> is a more recent, more sophisticated search engine,
which looks very powerful and should be probably faster and definitely
more scalable than C<Search::Indexer>; but also with a less compact
API. I haven't performed any detailed comparison yet.

=cut












