use strict;
use warnings;
use Data::Dumper;
use Carp;
use locale;


while (<STDIN>) {
  print STDERR Dumper(parseQuery($_));
}



sub parseQuery { # consumes tokens directly from $_[0]
  
  my %q = ('+' => [], '-' => [], '' => []);

  my $e = '';
  my $le = '';

  my $term = qr/\w+/;

 TOKEN:
  while (1) {
    for ($_[0]) {

      s/^\s+//;
      last TOKEN unless length;

      m/^\)/                and return \%q;

      s/^(\+|-)//           and do { $e = $1; next TOKEN };

      s/^(NOT|PAS)//        and do { $e = '-'; next TOKEN };

      s/^(AND|ET)//         and do { croak " AND after '$le'" if $le;
				     push @{$q{'+'}}, pop @{$q{''}}; 
				     $e = '+';
				     next TOKEN };

      s/^(OR|OU)//          and do { croak " OR after '$le'" if $le;
				     next TOKEN };

      s/^"\s*([^"]+?)\s*"// and do { my $s = $1;
				     push @{$q{$e}}, [$s =~ /$term/g ];
				     $le = $e; $e = '';
				     next TOKEN };

      s/^\(//               and do { my $r = &parseQuery; # implicit arg in @_
				     s/^\s*\)//  or croak "unmatched ( ";
				     push @{$q{$e}}, $r;
				     $le = $e; $e = '';
				     next TOKEN };

      s/^($term)//          and do { push @{$q{$e}}, $1; 
				     $le = $e; $e = '';
				     next TOKEN };



      croak "unexpected string in query : $_";
    }
  }

  return \%q;
}

