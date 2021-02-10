
#ifdef USE_STACK
#define temp(t, n, s) t, DIMENSION s :: n; POINTER (IP_##n##_, n)
#define init_stack() INTEGER :: ISTPT; ISTPT = KSTPT
#define alloc(n) IP_##n##_ = LOC (PSTACK (ISTPT)); ISTPT = ISTPT + SIZE (n); IF (ISTPT > KSTSZ) STOP "STACK OF: "//__FILE__
#else
#define temp(t, n, s) t, DIMENSION s :: n
#define alloc(n)
#define init_stack() INTEGER ::ISTPT
#endif
