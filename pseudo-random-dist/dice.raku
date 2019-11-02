# dice, probably over-engineered

grammar Dice {
    token TOP { \s* <times> 'd' <sides> [ <mod> ]? \s* }
    token times { \d+ }
    token sides { \d+ }
    token mod { <[+-]> \d+ }
}

my $string = "3d6+1";

my $d = Dice.parse($string);

class Result { has $.mod; has $.sum; has @.rolls; }

# TODO probably should be a method
sub roll (Dice $d) {
    my $m = $d<mod>.defined ?? $d<mod>.Int !! 0;
    my @rolls;
    for 1..$d<times> {
        push @rolls, 1 + $d<sides>.rand.Int;
    }
    my $sum = $m + [+] @rolls;
    Result.new(mod => $m, sum => $sum, rolls => @rolls);
}

say map { .rolls, .mod, .sum }, roll($d);
