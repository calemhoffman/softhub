#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstdlib>
#include <cstring>


using namespace std;

int get_num();

int main(int argc,char* argv[])
{


    int entry = get_num();


    fstream read_fort3;
    fstream w_data;


    read_fort3.open("fort.3",ios::in);
    w_data.open("fort3.dat",ios::out);

    if( read_fort3.fail() ) { cout << "cannot FIND fort.3" << endl; exit(1);}

    float angle[181];
    float chi_sqr[entry][181];

    // initialization.
    for(int j=0; j<entry; j++)
    {
        for(int i =0; i<181; i++)
        {
            chi_sqr[j][i] = 0;
            angle[i] = 0;
        }

    }


    int num;

    int buff;

    for(int j=0; j<entry; j++)
    {
        read_fort3 >> num >> buff >> buff >> buff >> buff;

        for(int i =0; i<181; i++)
        {
            read_fort3 >> angle[i] >> chi_sqr[j][i];
        }

    }

    read_fort3.close();

    //**********wirte out to fort3.dat file***************

    for(int i =0; i<181; i++)
    {
        //cout << fixed << setw(2)<<setprecision(0)<<angle[i]<<"\t";
        w_data << fixed << setw(2)<<setprecision(0)<<angle[i]<<"\t";
        for(int j=0; j<entry; j++)
        {
            w_data<<fixed<<setprecision(2)<<setw(7)<<chi_sqr[j][i]<<"\t";
            //cout<<fixed<<setprecision(2)<<setw(7)<<chi_sqr[j][i]<<"\t";

        }
        //cout << "\n";
        w_data << "\n";
    }

    w_data.close();


    return 0;
}

int get_num()
{

    fstream read_fort3;

    read_fort3.open("fort.3",ios::in);



    if( read_fort3.fail() ) { cout << "cannot FIND fort.3" << endl; exit(1);}

    int num;

    int buff;

    int count = 0;  // initialization.



    float* angle   = new float [181];
    float* chi_sqr = new float [181];



    while( !read_fort3.eof())
    {
        read_fort3 >> num >> buff >> buff >> buff >> buff;

        //cout << num << endl;
        //cout << buff << endl;


        count++;


        for(int i =0; i<181; i++)
        {
            read_fort3 >> angle[i] >> chi_sqr[i];
            //cout << angle[i] <<" "<<chi_sqr[i]<<endl;
        }


    }


    read_fort3.close();

    return count-1; // the while() will read twice the final run. so we need to substract 1.





}


