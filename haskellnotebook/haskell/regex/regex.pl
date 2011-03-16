#!/bin/perl

print "Running\n";

# Simple replace
$hs = "checking for needle in haystack --->";
$hs =~ s/needle/chicken/;
print $hs;
print "\n";

$string = "          sdf|       |sdf             ";
for ($string) {
	s/^\s+//;
	s/\s+$//;
	s/\s+/ /g;
}
print $string;
print "\n";

# Split a string into words

$line = "Dogs and cats have fun";
$line =~ s/^\s+//;
@array = split(/\W/, $line);

print $array[0];

print "\nDone\n";
