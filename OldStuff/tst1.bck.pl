use strict;
use warnings;
use Data::Dumper;
use Carp;


while (<STDIN>) {
  print STDERR Dumper(parseQuery($_));
}



sub parseQuery {
  my %q = ('+' => [], '-' => [], '' => []);

  print STDERR "Parsing $_[0]\n";

  my $e = '';
  my $le = '';

  my $term = qr/\w+/;

  while (1) {

      print STDERR "Loop $_[0]\n";

      $_[0] =~ s/^\s+//;
      last unless length($_[0]);

      $_[0] =~ s/^(\+|-)//           and do { $e = $1; next };

      $_[0] =~ s/^"\s*([^"]+?)\s*"// and do { my $s = $1;
				     push @{$q{$e}}, [$s =~ /$term/g ];
				     $le = $e; $e = '';
				     next };

      $_[0] =~ s/^\(//               and do { my $r = parseQuery(); 
				     s/^\s*\)//  or croak "unmatched ( ";
				     push @{$q{$e}}, $r;
				     $le = $e; $e = '';
				     next };

      $_[0] =~ s/^($term)//          and do {
	                             print STDERR "matched $1\n";
	                             push @{$q{$e}}, $1; 
				     $le = $e; $e = '';
				     next };

      $_[0] =~ s/^AND|ET//           and do { croak " AND after '$le'" if $le;
				     push @{$q{'+'}}, pop @{$q{''}}; 
				     $e = '+';
				     next };

      $_[0] =~ s/^OR|OU//            and do { croak " OR after '$le'" if $le;
				     next };


      croak "unexpected string in query : $_";
    }

  return \%q;
}

