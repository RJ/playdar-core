#!/usr/bin/perl
# pipe this to your script to simulate a request from playdar:
# ( ./mock-input.pl ; sleep 10 ) | myscript

$art = "mokele";
$trk = "hiding in your insides";
my $rq = "{ \"qid\":\"4e63397e-18a3-11de-ba95-9bb64e34b615\", \"artist\" : \"$art\", \"track\" : \"$trk\" }";
print pack('N', length($rq));
print $rq;
