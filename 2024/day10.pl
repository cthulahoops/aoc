#!/usr/bin/perl
use strict;
use warnings;

package Grid;

sub new {
    my ($class, $input_file) = @_;
    my $self = {
        grid => [],
        width => 0,
        height => 0
    };
    bless $self, $class;
    
    if (defined $input_file) {
        $self->load_from_file($input_file);
    }
    
    return $self;
}

sub load_from_file {
    my ($self, $filename) = @_;
    
    open(my $fh, '<', $filename) or die "Cannot open file '$filename': $!";
    while (my $line = <$fh>) {
        chomp $line;
        # Remove any whitespace
        $line =~ s/\s+//g;
        # Convert string to array of digits
        my @row = split //, $line;
        push @{$self->{grid}}, \@row;
    }
    close $fh;
    
    $self->{height} = scalar @{$self->{grid}};
    $self->{width} = scalar @{$self->{grid}[0]} if $self->{height} > 0;
}

sub get_element {
    my ($self, $point) = @_;
    my $x = $point->{x};
    my $y = $point->{y};
    
    return undef if $x < 0 || $x >= $self->{width} || 
                   $y < 0 || $y >= $self->{height};
    
    return $self->{grid}[$y][$x];
}

sub print_grid {
    my ($self) = @_;
    
    foreach my $row (@{$self->{grid}}) {
        print join('', @$row) . "\n";
    }
}

package Point;

use overload '""' => \&to_string;

sub new {
    my ($class, $x, $y) = @_;
    my $self = {
        x => $x,
        y => $y
    };
    bless $self, $class;
    return $self;
}

sub add {
    my ($self, $other) = @_;
    return Point->new($self->{x} + $other->{x}, $self->{y} + $other->{y});
}

sub x {
    my ($self) = @_;
    return $self->{x};
}

sub y {
    my ($self) = @_;
    return $self->{y};
}

sub to_string {
    my ($self) = @_;
    return "(" . $self->{x} . "," . $self->{y} . ")";
}

package main;

my @STEPS = (
    Point->new(1, 0),
    Point->new(0, 1),
    Point->new(-1, 0),
    Point->new(0, -1)
);

sub peaks_reachable {
    my ($grid, $point, $peaks, $trails) = @_;

    my $element = $grid->get_element($point);

    if ($element == 9) {
        $peaks->{$point} = 1;
        push @$trails, $point;
    }

    for my $step (@STEPS) {
        my $new = $point->add($step);

        my $new_element = $grid->get_element($new);
        if ($new_element && $new_element == $element + 1) {
            peaks_reachable($grid, $new, $peaks, $trails);
        }
    }
}

sub main {
    die "Usage: $0 grid_file\n" unless @ARGV == 1;
    my $input_file = $ARGV[0];

    my $grid = Grid->new($input_file);
    print "Grid contents:\n";
    $grid->print_grid();

    print $grid->{width} . " x " . $grid->{height} . " grid\n";

    print $grid->get_element(Point->new(4, 5)) . "\n";

    my $part1 = 0;
    my $part2 = 0;

    for (my $y = 0; $y < $grid->{height}; $y++) {
        for (my $x = 0; $x < $grid->{width}; $x++) {
            my $point = Point->new($x, $y);
            if ($grid->get_element($point) == 0) {
                my %peaks = ();
                my @trails = ();

                peaks_reachable($grid, $point, \%peaks, \@trails);
                print "Peaks reachable from $x,$y: ", join(", ", keys %peaks), " - ";
                print "Trails: ", scalar @trails, "\n";
                my $count1 = keys %peaks;
                $part1 += $count1;

                my $count2 = scalar @trails;
                $part2 += $count2;
            }
        }
    }

    print "Sum of peaks reachable: $part1\n";
    print "Sum of trails: $part2\n";
}

main();
