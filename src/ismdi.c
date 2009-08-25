void ismodemdi(int *a)
{
#ifdef WIN32
extern int GA_ismdi();
a[0] = GA_ismdi();
#else
a[0] = 0;
#endif
}
