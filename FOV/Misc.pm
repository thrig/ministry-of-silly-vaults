package Misc;
use 5.24.0;
use warnings;
use Exporter 'import';
use Math::Trig ':pi';

our @EXPORT = qw(at clear_screen t_hili t_norm ring_points);

# PORTABILITY - xterm control sequences - http://invisible-island.net/xterm/
sub at              { "\e[" . (1 + $_[1]) . ';' . (1 + $_[0]) . 'H' }
sub clear_screen () { "\e[1;1H\e[2J" }
sub t_hili ()       { "\e[41m" }
sub t_norm ()       { "\e[m" }

# NOTE can produce points that are off of the board
# NOTE probably should be cached for quicker lookup
sub ring_points {
    my ($radius, $swing, $x, $y) = @_;
    my $rf    = 0.5 + $radius;
    my $angle = 0;
    my (@points, %seen);
    while ($angle < pi2) {
        my $nx = $x + int($rf * cos $angle);
        my $ny = $y + int($rf * sin $angle);
        push @points, $nx, $ny unless $seen{ $nx . ',' . $ny }++;
        $angle += $swing;
    }
    return @points;
}

1;
