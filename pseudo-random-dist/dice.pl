#!/usr/bin/env perl
#
# a XdY+Z dice roller done a few different ways (or the same way done a
# few different times)

use 5.26.0;
use feature 'signatures';
use warnings;
no warnings 'experimental::signatures';
use Carp 'croak';
use List::Util 1.26 'sum0';

########################################################################
#
# closure, call the returned sub for a random roll

sub make_roller ($spec) {
    my ($times, $sides, $mod) = $spec =~ m[ (\d+) d (\d+) ([+-]\d+)? ]ax;
    croak("could not parse $spec") if !defined $times;
    $mod //= 0;
    sub { $mod + sum0(map { 1 + int rand $sides } 1 .. $times) }
}

my $cixa = make_roller('3d6-3');

say 'You hit the Troll for ' . $cixa->() . ' damage.';

########################################################################
#
# this version is akin to $RANDOM in ksh(1) where using the variable
# returns a different random value each time

use Variable::Magic qw(cast wizard);

sub set_random ($var, @spec) {
    cast $$var, wizard(
        get => sub ($var, @rest) {
            state $rolls = [ map { make_roller($_) } @spec ];
            state $n     = 0;
            $$var = $rolls->[ $n++ ]->();
            $n %= $rolls->@*
        }
    );
}
my $RANDOM;
set_random \$RANDOM, qw[1d8 1d8 2d6];

say "The Troll hits you for $RANDOM damage." for 1 .. 3;

########################################################################
#
# or of course OO, here with MOP for maximum overkill

use Class::MOP;
use Class::MOP::Attribute;

Class::MOP::Class->create(
    'Imp' => (
        attributes => [
            Class::MOP::Attribute->new(
                'name' => (reader => 'name', default => 'Bob')
            ),
            Class::MOP::Attribute->new(
                'roll' => (reader => 'roll', init_arg => 'roll')
            )
        ],
        methods => {
            attack => sub ($o) { $o->name . ' hits you for ' . $o->roll->() . '.' }
        }
    )
);

my $imp = Imp->meta->new_object(roll => make_roller('2d4+1'));
say $imp->attack;

########################################################################
#
# Moo is another OO option (see also Moo::Role)

package CowAttack {
    use Moo;
    has qw[roll is rwp];
    no warnings 'experimental::signatures';
    sub BUILD ($m, $args) { $m->_set_roll(main::make_roller($args->{dice})) }
    use overload '""' => sub ($m, @rest) { $m->roll->() }
}

my $ouch = CowAttack->new(dice => q!42d640!);

say "The Cow hits you for $ouch damage!!!";
die "You die..."
