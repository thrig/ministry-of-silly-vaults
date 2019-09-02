# -*- Perl -*-
#
# utility class that converts odds (input in the range 0..100) into
# various forms, e.g.
#
#   perl -I. -MOdds -E 'say Odds->new(odds=>shift)->float' 30
#   perl -I. -MOdds -E 'say Odds->new(odds=>shift)->all'   66

package Odds;

use 5.24.0;
use warnings;

use Moo;
use namespace::clean;

has odds   => ( is => 'rw' );
has float  => ( is => 'lazy' );
# from rogue 3.6.3 the very bad
#define RN (((seed = seed*11109+13849) & 0x7fff) >> 1)
has rogue  => ( is => 'lazy' );
has uint32 => ( is => 'lazy' );
has uint64 => ( is => 'lazy' );

sub BUILD { die "need odds" unless exists $_[1]->{odds} }
sub _build_float  { $_[0]->odds / 100 }
sub _build_rogue  { int( ( $_[0]->odds / 100 ) * 16383 ) }
sub _build_uint32 { int( ( $_[0]->odds / 100 ) * 4294967295 ) }
sub _build_uint64 { int( ( $_[0]->odds / 100 ) * 18446744073709551615 ) }

sub all { join $/, map $_[0]->$_, qw(odds float rogue uint32 uint64) }

1;
