git stash -q --keep-index
cabal test
RESULT=$?
git stash pop -q
[ $RESULT -ne 0 ] && exit 1
exit 0
