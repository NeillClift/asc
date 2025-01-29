// asc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//
#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <intrin.h>
#include <vector>
#include <math.h>

using namespace std;

typedef unsigned long long NTYPE;
typedef long long SNTYPE;
typedef int LPTYPER;
typedef int8_t LPTYPE;

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

struct TOKEN_TABLE {
    union {
        char diff[4];
        int all;
    } u;
    char value;
};

struct TOKEN_TABLE_COMPRESSED {
    union {
        char diff[4];
        int all;
    } u;
};

class lKnown {
    NTYPE Start, End;
    const size_t BufferSize = 1024 * 128;
    unsigned char* Buffer;
    FILE* fp;
    TOKEN_TABLE_COMPRESSED Lookup[256];
    unsigned int LookupSize;

public:

    char BadValue = -1;

    lKnown()
    {
        Buffer = new unsigned char[BufferSize];
        if ((fp = fopen("add39z.4ln", "rbR")) == NULL) {
            fp = nullptr;
            //			printf("can't open add39z.4ln\n");
            //			exit(0);
        }
        else {
            fread(&LookupSize, sizeof(LookupSize), 1, fp);
            if (LookupSize > 256) {
                printf("Bad lookup table size %u\n", LookupSize);
                exit(0);
            }
            fread(Lookup, sizeof(Lookup[0]), LookupSize, fp);
        }
        Start = (NTYPE)-1;
        End = (NTYPE)-2;
    }

    ~lKnown()
    {
        if (fp) {
            fclose(fp);
        }
        delete[] Buffer;
    }

    NTYPE Limit()
    {
        long long current = _ftelli64(fp);
        _fseeki64(fp, 0, SEEK_END);
        NTYPE limit = _ftelli64(fp);
        _fseeki64(fp, current, SEEK_SET);
        return (NTYPE)((limit - (LookupSize + 1) * 4) * 4 - 1);
    }

    NTYPE GetBufferSize()
    {
        return BufferSize * 4;
    }

    bool ShiftBuffer(NTYPE n)
    {
        if (n < Start || n > End) {
            if (fp == nullptr) {
                return false;
            }
            if (n != End + 1) {
                long long Offset = n / 4 + (LookupSize + 1) * 4;
                _fseeki64(fp, Offset, SEEK_SET);
                Start = n & ~(NTYPE)3;
            }
            else {
                Start = End + 1;
            }
            size_t len = fread(Buffer, sizeof(Buffer[0]), BufferSize, fp);
            if (len == 0) {
                return false;
            }
            End = Start + len * 4 - 1;
        }
        return true;
    }

    char s(NTYPE n)
    {
        if (!ShiftBuffer(n)) {
            return -1;
        }
        NTYPE diff = n - Start;
        unsigned char c = Buffer[diff / 4];
        unsigned int offset = diff % 4;
        if (c >= LookupSize) {
            printf("Bad token %d in data. Max is %u", c, LookupSize - 1);
            exit(0);
        }
        return (char)(log2u(bits(n)) + Lookup[c].u.diff[offset]);
    }

    char l(NTYPE n)
    {
        char ln = s(n);
        if (ln != -1) {
            ln += lambda(n);
        }
        return (char)ln;
    }
};

__declspec(thread) static lKnown lk;

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
            if (a < elem[c].a || (a == elem[c].a && s < elem[c].s)) {
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
    vector<LPTYPE> ln;
    ln.assign(MAX_TEST_VALUE + 2, -1);
    NTYPE LnFound = 0;

    do {
#pragma omp parallel
        {
#pragma omp for schedule(dynamic)
            for (SNTYPE n = Start; n <= (SNTYPE)Power; n++) {
                LPTYPER l = log2u(n);
                LPTYPER ll = lk.l(n);
                while (true) {
                    if (l == ll || FindChain(n, l)) {
#pragma omp critical
                        {
                            ln[n] = l;
                            if (LnFound + 1 == n) {
                                while (ln[LnFound + 1] != -1) {
                                    LnFound++;
                                    cout << LnFound << " " << (LPTYPER)ln[LnFound] << "\n";
                                }
                            }
                        }
                        break;
                    }
                    l++;
                }
            }
        }

        cerr << "//n <= 2^" << lambda(Power) << " " << (double)(clock() - start_time) / CLOCKS_PER_SEC << " secs.\n";
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
