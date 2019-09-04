# -*- Perl -*-
#
#define RN (((seed = seed*11109+13849) & 0x7fff) >> 1)
#
# which has an implicit % on whatever the size of the type of "seed" is,
# hopefully a power of 2

package RogueRnd;
use strict;
use integer;
use warnings;
use Variable::Magic qw(cast wizard);

sub make_rng {
    my ($ref) = @_;
    my $seed;
    cast $$ref, wizard(
        set => sub { my ($var) = @_; $seed = $$var % 32768 },
        get => sub {
            my ($var) = @_;
            $seed = ($seed * 11109 + 13849) % 32768;
            $$var = ($seed & 0x7fff) >> 1;
        }
    );
}

1;
