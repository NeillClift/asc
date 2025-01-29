// asc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <intrin.h>
#include <vector>
#include <math.h>

using namespace std;

typedef unsigned long long NTYPE;
typedef long long SNTYPE;
typedef int LPTYPER;

struct Elem {
	//
	// Addition chain element just for ordering
	//
	NTYPE a;
	//
	// Actual addition subtraction chain element
	//
	NTYPE s;
	//
	// Class A bounds
	//
	NTYPE ClassA;
};

#define MAX_TEST_VALUE ((NTYPE)1<<20)

#define FLAGS_SHOW_USAGE 0x1
#define FLAGS_TEST       0x2

static NTYPE Flags = 0;

static LPTYPER bits(NTYPE n)
/*++

Calculates the number of 1 bits in a number.

--*/
{
    return (LPTYPER)__popcnt64(n);
}

static LPTYPER bitsNAF(NTYPE n)
{
    NTYPE nh = n >> 1;
    NTYPE n3 = n + nh;
    return bits(n3 ^ nh);
}

static LPTYPER lambda(NTYPE n)
/*++

Calculates the floor(log2(n)) of a number

--*/
{
    return (LPTYPER)(sizeof(n) * CHAR_BIT - 1 - _lzcnt_u64(n));
}

static LPTYPER log2u(NTYPE n)
/*++

Calculates the ceil(log2(n)) of a number

--*/
{
    int l = lambda(n);
    if ((n & (n - 1)) != 0) {
        l++;
    }
    return l;
}

static void do_usage(void)
/*++
Prints out the help
--*/
{
    cout << "asc [number]\n";
    cout << "asc -t    run test pass\n";
}

static void process_parameters(int argc, char* argv[], NTYPE& Target)
/*++
Parses the command line parameters
--*/
{
    char c, c1, *p;
    bool GotTarget = false;

    Flags = 0;
    Target = 0;

    while (--argc) {
        p = *++argv;
        if (*p == '/' || *p == '-') {
            while ((c = *++p) != 0) {
                c1 = (char)toupper(*(p + 1));
                if (c1) {
                    p++;
                }
                switch (toupper(c)) {
                case 'T':
                    Flags |= FLAGS_TEST;
                    break;
                case 'H':
                default:
                    Flags = FLAGS_SHOW_USAGE;
                    return;
                }
            }
        }
        else {
            if (!GotTarget) {
                Target = _atoi64(p);
                GotTarget = true;
            }  else {
                Flags = FLAGS_SHOW_USAGE;
                return;
            }
        }
    }
}

static void BuildBounds(vector<Elem> &elem, NTYPE n, LPTYPER l)
{
    for (LPTYPER i = l; i >= 0; i--) {
        elem[i].ClassA = n;
        n = (n + 1) / 2;
    }
}

static bool Recurse(vector<Elem> &elem, LPTYPER c, LPTYPER l, NTYPE n, NTYPE Refs, NTYPE RefsTotal)
{
    if (c == l) {
        if (elem[c].s == n) {
            if ((Flags & FLAGS_TEST) == 0) {
                for (auto& e : elem) {
                    cout << e.s << " ";
                }
                cout << "\n";
            }
            return true;
        }
        return false;
    }
    _bittestandset64((long long*)&Refs, c);
    RefsTotal += elem[c].s;
    LPTYPER Forward = c + bits(Refs) - 1;
    if (Forward > l || elem[Forward].ClassA > RefsTotal) {
        return false;
    }

    for (int j = c; j >= 0; j--) {
        for (int k = j; k >= 0; k--) {
            NTYPE NewRefs = Refs;
            NTYPE NewRefsTotal = RefsTotal;
            if (_bittestandreset64((long long *)&NewRefs, j)) {
                _ASSERT(NewRefsTotal >= elem[j].s);
                NewRefsTotal -= elem[j].s;
            }
            if (_bittestandreset64((long long*)&NewRefs, k)) {
                _ASSERT(NewRefsTotal >= elem[k].s);
                NewRefsTotal -= elem[k].s;
            }
            NTYPE a = elem[j].a + elem[k].a;
            NTYPE s = elem[j].s + elem[k].s;
            if (a < elem[c].a || (a == elem[c].a && s > elem[c].s)) {
                if (j == k) {
                    return false;
                }
                break;
            }
            elem[c + 1].a = a;
            elem[c + 1].s = s;
            if (Recurse(elem, c + 1, l, n, NewRefs, NewRefsTotal)) {
                return true;
            }

            elem[c + 1].a = a;
            elem[c + 1].s = llabs(elem[j].s - elem[k].s);
            if (elem[c + 1].s > 2) {
                if (Recurse(elem, c + 1, l, n, NewRefs, NewRefsTotal)) {
                    return true;
                }
            }
        }
    }
    return false;
}

static bool FindChain(NTYPE n, LPTYPER l)
{
    vector<Elem> elem;
    elem.assign(l + 1, Elem());
    BuildBounds(elem, n, l);
    elem[0].a = 1;
    elem[0].s = 1;
    NTYPE Refs = 0;
    NTYPE RefsTotal = 0;
    return Recurse(elem, 0, l, n, Refs, RefsTotal);
}

static void do_test()
{
    NTYPE Power = (NTYPE)1 << 11;
    NTYPE Start = 1;
    clock_t start_time = clock();
    LPTYPER ms = -1;
    do {
#pragma omp parallel
        {
#pragma omp for schedule(dynamic)
            for (SNTYPE n = Start; n <= (SNTYPE)Power; n++) {
                LPTYPER l = log2u(n);
                LPTYPER s = log2u(bitsNAF(n));
                LPTYPER lu = lambda(n) + s;
                if (s > ms) {
#pragma omp critical
                    {
                        if (s > ms) {
                            cout << "Hit " << s << " small steps for " << n << "\n";
                            ms = s;
                        }
                    }
                }
                while (l < lu) {
                    if (FindChain(n, l)) {
#pragma omp critical
                        {
                            cout << n << " " << l << "\n";
                            exit(0);
                        }
                        break;
                    }
                    l++;
                }
            }
        }

        cout << "//n <= 2^" << lambda(Power) << " " << (double)(clock() - start_time) / CLOCKS_PER_SEC << " secs.\n";
        Start = Power + 1;
        Power <<= 1;
    } while (Power <= MAX_TEST_VALUE);
}


int main(int argc, char *argv[])
{
    NTYPE n;
    process_parameters(argc, argv, n);

    if (Flags & FLAGS_TEST) {
        do_test();
        exit(0);
    }
    LPTYPER l = log2u(n);
    while (true) {
        if (FindChain(n, l)) {
            break;
        }
        l++;
    }
}
