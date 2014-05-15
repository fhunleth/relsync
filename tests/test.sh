#!/bin/sh

set -e

TESTS_DIR=$(dirname $(readlink -f $0))
RELSYNC=$TESTS_DIR/../relsync

WORKDIR=$TESTS_DIR/work
RELRW=$WORKDIR/rel
HOSTNAME=$(hostname)

do_sha1() {
    DIR=$1
    OUT=$2
    cd $DIR && find . -type f -or -type l | grep -v "/log/" | xargs sha1sum | sort -k 1 > $OUT
}

# clean up any old work
rm -fr $WORKDIR
mkdir -p $WORKDIR

# Make the test apps
make -C $TESTS_DIR/testapp1 clean
make -C $TESTS_DIR/testapp2 clean
make -C $TESTS_DIR/testapp1 all
make -C $TESTS_DIR/testapp2 all

do_sha1 $TESTS_DIR/testapp1/_rel $WORKDIR/testapp1.orig.sha1
do_sha1 $TESTS_DIR/testapp2/_rel $WORKDIR/testapp2.orig.sha1

echo "Starting testapp..."

# Start up the Erlang VM running testapp1
$TESTS_DIR/testapp1/_rel/bin/testapp start

echo "Waiting for startup..."

# sleep a little to make sure that everything is running
sleep 2

# Do a relsync with the second test app
$RELSYNC -h $TESTS_DIR/test_hooks.erl -d relsync_foo@$HOSTNAME -p $TESTS_DIR/testapp1/_rel -q $RELRW -l $TESTS_DIR/testapp2/_rel -s relsync -c cookie

# Exit the VM
$TESTS_DIR/testapp1/_rel/bin/testapp stop

# Create sha1's of the files
do_sha1 $TESTS_DIR/testapp1/_rel $WORKDIR/testapp1.after.sha1
do_sha1 $TESTS_DIR/testapp2/_rel $WORKDIR/testapp2.after.sha1
do_sha1 $RELRW $WORKDIR/relrw.sha1

echo Checking that the testapp1 and testapp2 directories were not modified...
diff $WORKDIR/testapp1.orig.sha1 $WORKDIR/testapp1.after.sha1 || (echo "Error: testapp1 modified"; exit 1)
diff $WORKDIR/testapp2.orig.sha1 $WORKDIR/testapp2.after.sha1 || (echo "Error: testapp2 modified"; exit 1)

echo Checking that the updated release directory matches testapp2...
diff $WORKDIR/testapp2.after.sha1 $WORKDIR/relrw.sha1 || (echo "Error: sync'd directory wrong"; exit 1)

echo Checking that the log has the right contents...
grep "Hello from testapp1" $TESTS_DIR/testapp1/_rel/log/erlang.log.1 > /dev/null || (echo "Error: hello testapp1 text missing"; exit 1)
grep "Hello from testapp2" $TESTS_DIR/testapp1/_rel/log/erlang.log.1 > /dev/null || (echo "Error: hello testapp2 text missing"; exit 1)

echo "Success"
