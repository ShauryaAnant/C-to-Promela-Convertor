proctype main() {
    int a = 5;
    int b = 10;

    if
    :: (a < b) -> a = a + 1;
    :: else -> skip;
    fi;

    do
    :: (a < b) -> a = a + 1;
    :: else -> break;
    od;

    return;
}
