Playdar Contrib Dir
===================
Please put your scripts and experimental stuff in here.
As things mature we will consider moving some into a top level
scripts directory for good, stable resolvers.

Trying a resolver
-----------------
To use a resolver script, add it to the main playdar.conf.

Testing your script
-------------------
To test a resolver script you can run:

( ./mock-input.pl ; sleep 10 ) | ./your/script.sh

Edit mock-input to search for something your script should find.

Or just make Playdar use it and see what happens.
