# utility class to convert odds (input in the range 0..100) into various forms
#   use Odds;
#   my $o = Odds->new(30);
#   $o->float; # .30 (or so)
package Odds;
use Moo;
use namespace::clean;
has odds   => ( is => 'rw' );
has float  => ( is => 'lazy' );
has uint32 => ( is => 'lazy' );
sub BUILD { die "need odds" unless exists $_[1]->{odds} }
sub _build_float { $_[0]->odds / 100 }
sub _build_uint32 { int( ( $_[0]->odds / 100 ) * 4294967296 ) }
1;
