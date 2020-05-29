#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <cstring>

using namespace  std;

int main(int argc, char* argv[])
{

    int input = atoi(argv[1]);

    char letter = '\0';

    if     (input == 1 ) { letter = 'a';}
    else if(input == 2 ) { letter = 'b';}
    else if(input == 3 ) { letter = 'c';}
    else if(input == 4 ) { letter = 'd';}
    else if(input == 5 ) { letter = 'e';}
    else if(input == 6 ) { letter = 'f';}
    else if(input == 7 ) { letter = 'g';}
    else if(input == 8 ) { letter = 'h';}
    else if(input == 9 ) { letter = 'i';}
    else { letter = '\0';}

    cout << letter;


}
