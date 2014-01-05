(in-package #:naclcl)

(defun sub-vector (vector start end)
  (make-array (- end start)
              :displaced-to vector
              :displaced-index-offset start))

; #include "tweetnacl.h"
; #define FOR(i,n) for (i = 0; i < n; ++i)
; #define sv static void

; typedef unsigned char u8;
; typedef unsigned int u32;
; typedef unsigned long long u64;
; typedef long long i64;
; typedef i64 gf[16]; //16x64 = 1024

(deftype -u8- () 'unsigned-byte)
(deftype -u32- () 'integer) ; to be specified
(deftype -u64- () 'integer)
(deftype -i64- () 'integer)

(defun make-gf ()
  (make-array 16 :element-type '-i64- :initial-element 0))

(defun make-gf-1 ()
  (let ((gf (make-gf)))
    (setf (aref gf 0) 1)
    gf))

; extern void randombytes(u8*, u64);

(defun randombytes (n &optional (target (make-array n :element-type '-u8-)))
  (map-into target
            (lambda ()
              (random 256))))

; static const u8 _0[16],
;                 _9[32] = {9};
(defparameter *0* (make-array 16 :element-type '-u8-))
(defparameter *9* (make-array 32
                              :element-type '-u8-
                              :initial-contents (cons 9
                                                      (loop :repeat 31
                                                            :collect 0))))

; static const gf gf0,
;                 gf1={1},
;                 _121665={0xDB41,1},
;                 D={0x78a3,0x1359,0x4dca,0x75eb,0xd8ab,0x4141,0x0a4d,0x0070,0xe898,0x7779,0x4079,0x8cc7,0xfe73,0x2b6f,0x6cee,0x5203},
;                 D2={0xf159,0x26b2,0x9b94,0xebd6,0xb156,0x8283,0x149a,0x00e0,0xd130,0xeef3,0x80f2,0x198e,0xfce7,0x56df,0xd9dc,0x2406},
;                 X={0xd51a,0x8f25,0x2d60,0xc956,0xa7b2,0x9525,0xc760,0x692c,0xdc5c,0xfdd6,0xe231,0xc0a4,0x53fe,0xcd6e,0x36d3,0x2169},
;                 Y={0x6658,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666,0x6666},
;                 I={0xa0b0,0x4a0e,0x1b27,0xc4ee,0xe478,0xad2f,0x1806,0x2f43,0xd7a7,0x3dfb,0x0099,0x2b4d,0xdf0b,0x4fc1,0x2480,0x2b83};

(defparameter *gf0* (make-gf))
(defparameter *gf1* (make-gf-1))

(defparameter *121665* (make-array 16
                                   :element-type '-i64-
                                   :initial-contents (list* 1
                                                            #xDB41
                                                            (loop :repeat 14
                                                                  :collect 0))))

(defparameter *d* #(#x78a3 #x1359 #x4dca #x75eb
                     #xd8ab #x4141 #x0a4d #x0070
                     #xe898 #x7779 #x4079 #x8cc7
                     #xfe73 #x2b6f #x6cee #x5203))
(defparameter *d2* #(#xf159 #x26b2 #x9b94 #xebd6
                     #xb156 #x8283 #x149a #x00e0
                     #xd130 #xeef3 #x80f2 #x198e
                     #xfce7 #x56df #xd9dc #x2406))
(defparameter *x* #(#xd51a #x8f25 #x2d60 #xc956
                    #xa7b2 #x9525 #xc760 #x692c
                    #xdc5c #xfdd6 #xe231 #xc0a4
                    #x53fe #xcd6e #x36d3 #x2169))
(defparameter *y* #(#x6658 #x6666 #x6666 #x6666
                    #x6666 #x6666 #x6666 #x6666
                    #x6666 #x6666 #x6666 #x6666
                    #x6666 #x6666 #x6666 #x6666))
(defparameter *i* #(#xa0b0 #x4a0e #x1b27 #xc4ee
                    #xe478 #xad2f #x1806 #x2f43
                    #xd7a7 #x3dfb #x0099 #x2b4d
                    #xdf0b #x4fc1 #x2480 #x2b83))

; static u32 L32 (u32 x, int c)
; { return (x << c) | (x >> (32 - c)); }

(defun l-32 (x c)
  (declare (type -u32- x)
           (type integer c))
  (logior (ash x c)
          (ash x (- (- 32 c)))))

; static u32 ld32 (const u8 *x)
; { u32 u = x[3];
;   u = (u<<8) | x[2];
;   u = (u<<8) | x[1];
;   return (u<<8) | x[0]; }

(defun ld-32 (x)
  (declare ((array -u8-) x))
  (logior (ash (aref x 3) (* 3 8))
          (ash (aref x 2) (* 2 8))
          (ash (aref x 1) (* 1 8))
          (aref x 0)))

; static u64 dl64 (const u8 *x)
; { u64 i, u=0;
;   FOR (i, 8) u = (u<<8) | x[i];
;   return u; }

(defun dl-64 (array)
  (assemble-number array 8))

(defun assemble-number (array n)
  "Assemble array to a single number.  Array consists of bytes, most
significant last."
  (declare ((array -u8-) array))
  (let ((target 0))
    (dotimes (i n)
      (setf target
            (dpb (aref array i)
                 (byte 8 (* i 8))
                 target)))
    target))

; sv st32 (u8 *x, u32 u)
; { int i;
;   FOR (i, 4)
;   { x [i] = u;
;     u >>= 8; } }

(defun st-32 (number)
  (declare (type -u32- number))
  (disassemble-number number 4))

(defun disassemble-number (number size)
  (let ((target (make-array size :element-type '-u8-)))
    (loop :for i :below size
          :for position :upfrom 0 :by 8
          :do (setf (aref target i)
                    (ldb (byte 8 position) number)))))

; sv ts64 (u8 *x, u64 u)
; { int i;
;   for (i = 7; i >= 0; --i)
;   { x[i] = u;
;     u >>= 8; } }

(defun ts-64 (number)
  (declare (type -u64- number))
  (disassemble-number number 8))

; static int vn (const u8 *x, const u8 *y, int n)
; { u32 i, d = 0;
;   FOR (i, n) d |= x[i] ^ y[i];
;   return (1 & ((d - 1) >> 8)) - 1; }

(defun vn (array-x array-y)
  "Vergleich mit konstantem Zeitbedarf (d. h. immer gleich, um aus der
Vergleichszeit keinen Side-Channel-Angriff zu ermöglichen.  Gibt 0
zurück, wenn die Arrays gleich sind, sonst -1."
  (let ((combined (reduce #'logior
                          (map 'vector #'logxor array-x array-y))))
    (1- (logand 1 (ash (1- combined) -8)))))

; int crypto_verify_16 (const u8 *x, const u8 *y)
; { return vn (x, y, 16); }

(defun crypto-verify-16 (x y)
  (zerop (vn x y)))

(defun crypto-verify-32 (x y)
  (zerop (vn x y)))

; sv core (u8 *out, const u8 *in, const u8 *k, const u8 *c, int h)
; { u32 w[16], x[16], y[16], t[4];
;   int i, j, m;
;   FOR (i, 4)
;   { x[5*i] = ld32 (c + 4 * i);
;     x[1+i] = ld32 (k + 4 * i);
;     x[6+i] = ld32 (in + 4 * i);
;     x[11+i] = ld32 (k + 16 + 4 * i); }
;   FOR (i, 16) y[i] = x[i];
;   FOR (i, 20)
;   { FOR (j, 4)
;     { FOR (m, 4) t[m] = x[(5*j+4*m)%16];
;       t[1] ^= L32 (t[0] + t[3], 7);
;       t[2] ^= L32 (t[1] + t[0], 9);
;       t[3] ^= L32 (t[2] + t[1], 13);
;       t[0] ^= L32 (t[3] + t[2], 18);
;       FOR (m, 4) w[4*j+(j+m)%4] = t[m]; }
;     FOR (m, 16) x[m] = w[m]; }
;   if (h)
;   { FOR (i, 16) x[i] += y[i];
;     FOR (i, 4)
;     { x[5*i] -= ld32 (c+4*i);
;       x[6+i] -= ld32 (in+4*i); }
;     FOR (i, 4)
;     { st32 (out + 4 * i, x[5*i]);
;       st32 (out + 16 + 4 * i, x[6+i]); } }
;   else
;     FOR (i, 16) st32 (out + 4 * i, x[i] + y[i]); }

(defun core (out in k c h
             &aux
               (w (make-array 16 :element-type '-u32-))
               (x (make-array 16 :element-type '-u32-))
               (y (make-array 16 :element-type '-u32-))
               (tmp (make-array 16 :element-type '-u32-)))
  (declare (array out in k c)
           (boolean h))
  (dotimes (i 4)
    (let* ((start-index (* 4 i))
           (end-index (+ start-index 4)))
      (setf (aref x (* i 5)) (ld-32 (subseq c start-index end-index))
            (aref x (+ i 1)) (ld-32 (subseq k start-index end-index))
            (aref x (+ i 6)) (ld-32 (subseq in start-index end-index))
            (aref x (+ i 11)) (ld-32 (subseq k
                                             (+ 16 start-index)
                                             (+ 16 end-index))))))
  (map-into y #'identity x)
  (dotimes (i 20)
    (dotimes (j 4)
      (dotimes (m 4)
        (setf (aref tmp m)
              (aref x (mod (+ (* 5 j)
                              (* 4 m))
                           16))))
      (setf (aref tmp 1) (logxor (aref tmp 1)
                                 (l-32 (+ (aref tmp 0) (aref tmp 3))
                                       7))
            (aref tmp 2) (logxor (aref tmp 2)
                                 (l-32 (+ (aref tmp 1) (aref tmp 0))
                                       9))
            (aref tmp 3) (logxor (aref tmp 3)
                                 (l-32 (+ (aref tmp 2) (aref tmp 1))
                                       13))
            (aref tmp 0) (logxor (aref tmp 0)
                                 (l-32 (+ (aref tmp 3) (aref tmp 2))
                                       18)))
      (dotimes (m 4)
        (setf (aref w (mod (+ (* 4 j) (+ j m)) 4))
              (aref tmp m))))
    (setf x (copy-seq w)))
  (if h
      (progn
        (map-into x #'+ x y)
        (dotimes (i 4)
          (let* ((start-index (* 4 i))
                 (end-index (+ start-index 4)))
            (decf (aref x (* 5 i)) (ld-32 (subseq c start-index end-index)))
            (decf (aref x (+ i 6)) (ld-32 (subseq in start-index end-index)))))
        (dotimes (i 4)
          (let* ((start-index (* 4 i))
                 (end-index (+ start-index 4)))
            (setf (subseq out
                          start-index
                          end-index)
                  (st-32 (aref x (* 5 i)))
                  (subseq out
                          (+ start-index 16)
                          (+ end-index 16))
                  (st-32 (aref x (+ i 6)))))))
      (dotimes (i 16)
        (let* ((start-index (* 4 i))
               (end-index (+ start-index 4)))
          (setf (subseq out start-index end-index)
                (st-32 (+ (aref x i) (aref y i)))))))
  out)

; int crypto_core_salsa20 (u8 *out, const u8 *in, const u8 *k, const u8 *c)
; { core (out, in, k, c, 0);
;   return 0; }

(defun crypto-core-salsa-20 (out in k c)
  (core out in k c nil))

; int crypto_core_hsalsa20 (u8 *out, const u8 *in, const u8 *k, const u8 *c)
; { core (out, in, k, c, 1);
;   return 0; }

(defun crypto-core-hsalsa-20 (out in k c)
  (core out in k c t))

; static const u8 sigma[16]="expand 32-byte k";

(defparameter *sigma* "expand 32-byte k")

; int crypto_stream_salsa20_xor (u8 *c, const u8 *m, u64 b, const u8 *n, const u8 *k)
; { u8 z[16], x[64];
;   u32 u, i;
;   if (!b) return 0;
;   FOR (i, 16) z[i] = 0;
;   FOR (i, 8) z[i] = n[i];
;   while (b >= 64)
;   { crypto_core_salsa20 (x, z, k, sigma);
;     FOR (i, 64) c[i] = (m ? m[i] : 0) ^ x[i];
;     u = 1;
;     for (i = 8; i < 16; ++i)
;     { u += (u32) z[i];
;       z[i] = u;
;       u >>= 8; }
;     b -= 64;
;     c += 64;
;     if (m) m += 64; }
;   if (b) 
;   { crypto_core_salsa20 (x, z, k, sigma);
;     FOR (i, b) c[i] = (m ? m[i] : 0) ^ x[i]; }
;   return 0; }

(defun crypto-stream-salsa-20-xor (c m b n k
                                   &aux
                                     (x (make-array 64 :element-type '-u8-))
                                     (z (make-array 16
                                                    :element-type '-u8-
                                                    :initial-element 0)))
  (unless (zerop b)
    (setf (subseq z 0 8) (subseq n 0 8))
    (loop
      :while (>= b 64)
      :do (let ((x (crypto-core-salsa-20 x z k *sigma*))
                (u 1))
            (if m
                (map-into c #'logxor m x)
                (map-into c #'identity x))
            (loop
              :for i :from 8 :below 16
              :do (incf u (aref z i))
                  (setf (aref z i) u)
                  (setf u (ash u -8)))
            (decf b 64)
            (setf c (subseq c 64))
            (when m (setf m (subseq m 64)))))
    (unless (zerop b)
      (let ((x (crypto-core-salsa-20 x z k *sigma*)))
        (if m
            (map-into c #'logxor m x)
            (map-into c #'identity x)))))
  c)

; int crypto_stream_salsa20 (u8 *c, u64 d, const u8 *n, const u8 *k)
; { return crypto_stream_salsa20_xor (c, 0, d, n, k); }

(defun crypto-stream-salsa-20 (c d n k)
  (crypto-stream-salsa-20-xor c nil d n k))

; int crypto_stream (u8 *c, u64 d, const u8 *n, const u8 *k)
; { u8 s[32];
;   crypto_core_hsalsa20 (s,n,k,sigma);
;   return crypto_stream_salsa20 (c, d, n + 16, s); }

(defun crypto-stream (c d n k
                      &aux
                        (s (make-array 32 :element-type '-u8-)))
  (let ((s (crypto-core-hsalsa-20 s n k *sigma*)))
    (crypto-stream-salsa-20 c d (subseq n 16 24) s)))

; int crypto_stream_xor (u8 *c, const u8 *m, u64 d, const u8 *n, const u8 *k)
; { u8 s[32];
;   crypto_core_hsalsa20 (s,n,k,sigma);
;   return crypto_stream_salsa20_xor (c, m, d, n + 16, s); }

(defun crypto-stream-xor (c m d n k
                          &aux
                            (s (make-array 32 :element-type '-u8-)))
  (let ((s (crypto-core-hsalsa-20 s n k *sigma*)))
    (crypto-stream-salsa-20-xor c m d (subseq n 16 24) s)))

; sv add1305 (u32 *h, const u32 *c)
; { u32 j, u = 0;
;   FOR (j, 17)
;   { u += h[j] + c[j];
;     h[j] = u & 255;
;     u >>= 8; } }

(defun add-1305 (h c)
  (let ((u 0))
    (dotimes (j 17 h)
      (incf u (+ (aref h j) (aref c j)))
      (setf (aref h j) (logand u 255)
            u (ash u -8)))))

; static const u32 minusp[17] = {5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,252};

(defparameter *minusp* (make-array 17
                                   :element-type '-u32-
                                   :initial-contents (append (list 5)
                                                             (loop :repeat 15
                                                                   :collect 0)
                                                             (list 252))))

; int crypto_onetimeauth (u8 *out, const u8 *m, u64 n, const u8 *k)
; { u32 s, i, j, u, x[17], r[17], h[17], c[17], g[17];
;   FOR (j, 17) r[j] = h[j] = 0;
;   FOR (j, 16) r[j] = k[j];
;   r[3] &= 15;
;   r[4] &= 252;
;   r[7] &= 15;
;   r[8] &= 252;
;   r[11] &= 15;
;   r[12] &= 252;
;   r[15] &= 15;
;   while (n > 0)
;   { FOR (j, 17) c[j] = 0;
;     for (j = 0; (j < 16) && (j < n); ++j) c[j] = m[j];
;     c[j] = 1;
;     m += j;
;     n -= j;
;     add1305 (h, c);
;     FOR (i, 17)
;     { x[i] = 0;
;       FOR (j, 17) x[i] += h[j] * ((j <= i) ? r[i - j] : 320 * r[i + 17 - j]); }
;     FOR (i, 17) h[i] = x[i];
;     u = 0;
;     FOR (j, 16)
;     { u += h[j];
;       h[j] = u & 255;
;       u >>= 8; }
;     u += h[16];
;     h[16] = u & 3;
;     u = 5 * (u >> 2);
;     FOR (j, 16)
;     { u += h[j];
;       h[j] = u & 255;
;       u >>= 8; }
;     u += h[16];
;     h[16] = u; }
;   FOR (j, 17) g[j] = h[j];
;   add1305 (h, minusp);
;   s = -(h[16] >> 7);
;   FOR (j, 17) h[j] ^= s & (g[j] ^ h[j]);
;   FOR (j, 16) c[j] = k[j + 16];
;   c[16] = 0;
;   add1305 (h, c);
;   FOR (j, 16) out[j] = h[j];
;   return 0; }

(defun crypto-onetimeauth (out m n k)
  (let ((r (make-array 17 :element-type '-u32- :initial-element 0))
        (h (make-array 17 :element-type '-u32- :initial-element 0)))
    (setf (subseq r 0 16) (subseq k 0 16))
    (dolist (index '(3 7 11 15))
      (setf (aref r index) (logand (aref r index) 15)))
    (dolist (index '(4 8 12))
      (setf (aref r index) (logand (aref r index) 252)))
    (loop :while (plusp n)
      :for c := (make-array 17 :element-type '-u32- :initial-element 0)
      :for length := (min 16 n)
      :for x := (make-array 17 :element-type '-u32- :initial-element 0)
      :for u := 0
      :do (setf (subseq c 0 length) (subseq m 0 length)
                (aref c length) 1
                m (subseq m length))
          (decf n length)
          (setf h (add-1305 h c))
          (dotimes (i 17)
            (dotimes (j 17)
              (incf (aref x i)
                    (* (aref h j)
                       (if (<= j i)
                           (aref r (- i j))
                           (* 320 (aref r (- i -17 j))))))))
          (setf h x)
          (dotimes (j 16)
            (incf u (aref h j))
            (setf (aref h j) (logand u 255)
                  u (ash u -8)))
          (incf u (aref h 16))
          (setf (aref h 16) (logand u 3)
                u (* 5 (ash u -2)))
          (dotimes (j 16)
            (incf u (aref h j))
            (setf (aref h j) (logand u 255)
                  u (ash u -8)))
          (incf u (aref h 16))
          (setf (aref h 16) u)
      :finally
      (let ((g (copy-seq h)))
        (setf h (add-1305 h *minusp*))
        (let ((s (- (ash (aref h 16) -7))))
          (map-into h
                    (lambda (gval hval)
                      (logxor hval
                              (logand s
                                      (logxor gval hval))))
                    g h)))
      (setf (subseq c 0 16) (subseq k 16 32)
            (aref c 16) 0
            h (add-1305 h c)))
    (map-into out #'identity h)))

; int crypto_onetimeauth_verify (const u8 *h, const u8 *m, u64 n, const u8 *k)
; { u8 x[16];
;   crypto_onetimeauth(x,m,n,k);
;   return crypto_verify_16(h,x); }

(defun crypto-onetimeauth-verify (h m n k
                                  &aux
                                    (x (make-array 16 :element-type '-u8-)))
  (let ((x (crypto-onetimeauth x m n k)))
    (crypto-verify-16 h x)))

; int crypto_secretbox (u8 *c, const u8 *m, u64 d, const u8 *n, const u8 *k)
; { int i;
;   if (d < 32) return -1;
;   crypto_stream_xor (c, m, d, n, k);
;   crypto_onetimeauth (c + 16, c + 32, d - 32, c);
;   FOR (i, 16) c[i] = 0;
;   return 0; }

(defun crypto-secretbox (c m d n k)
  (when (< d 32) 
    (error "D should be >= 32, but is ~s." d))
  (crypto-stream-xor c m d n k)
  (crypto-onetimeauth (subseq c 16) (subseq c 32) (- d 32) c)
  (dotimes (i 16)
    (setf (aref c i) 0))
  c)

; int crypto_secretbox_open (u8 *m, const u8 *c, u64 d, const u8 *n, const u8 *k)
; { int i;
;   u8 x[32];
;   if (d < 32) return -1;
;   crypto_stream (x, 32, n, k);
;   if (crypto_onetimeauth_verify (c + 16, c + 32, d - 32, x) != 0) return -1;
;   crypto_stream_xor (m, c, d, n, k);
;   FOR (i, 32) m[i] = 0;
;   return 0; }

(defun crypto-secretbox-open (m c d n k
                              &aux
                                (x (make-array 32 :element-type '-u8-)))
  (when (< d 32) 
    (error "D should be >= 32, but is ~s." d))
  (let ((x (crypto-stream x 32 n k)))
    (assert (crypto-onetimeauth-verify (subseq c 16) (subseq c 32) (- d 32) x)
            ()
            "Verification failed.")
    (crypto-stream-xor m c d n k)
    (dotimes (i 32)
      (setf (aref m i) 0))
    m))

; sv set25519 (gf r, const gf a)
; { int i;
;   FOR (i, 16) r[i] = a[i]; }

(defun set-25519 (r a)
  (setf (subseq r 0 16)
        (subseq a 0 16))
  r)

; sv car25519 (gf o)
; { int i;
;   i64 c;
;   FOR (i, 16)
;   { o[i] += (1 << 16);
;     c = o[i] >> 16;
;     o[(i + 1) * (i < 15)] += c - 1 + 37 * (c - 1) * (i == 15);
;     o[i] -= c << 16; } }

(defun car-25519 (o)
  (dotimes (i 16 o)
    (incf (aref o i) (ash 1 16))
    (let ((c (ash (aref o i) -16)))
      (incf (aref o (* (1+ i)
                       (if (< i 15) 1 0)))
            (+ c -1 (* 37
                       (1- c)
                       (if (= i 15) 1 0))))
      (decf (aref o i) (ash c 16)))))

; sv sel25519 (gf p, gf q, int b)
; { i64 t, i, c = ~(b - 1);
;   FOR (i, 16)
;   { t = c & (p[i] ^ q[i]);
;     p[i] ^= t;
;     q[i] ^= t; } }

(defun sel-25519 (p q b)
  (let ((c (lognot (1- b))))
    (dotimes (i 16)
      (let ((tmp (logand c (logxor (aref p i) (aref q i)))))
        (setf (aref p i) (logxor (aref p i) tmp)
              (aref q i) (logxor (aref q i) tmp))))
    (values p q)))

; sv pack25519 (u8 *o, const gf n)
; { int i, j, b;
;   gf m, t;
;   FOR (i, 16) t[i] = n[i];
;   car25519 (t);
;   car25519 (t);
;   car25519 (t);
;   FOR (j, 2)
;   { m[0] = t[0] - 0xffed;
;     for (i = 1; i < 15; i++)
;     { m[i] = t[i] - 0xffff - ((m[i - 1] >> 16) & 1);
;       m[i - 1] &= 0xffff; }
;     m[15] = t[15] - 0x7fff - ((m[14] >> 16) & 1);
;     b = (m[15] >> 16) & 1;
;     m[15] &= 0xffff;
;     sel25519 (t, m, 1 - b); }
;   FOR (i, 16)
;   { o[2 * i] = t[i] & 0xff;
;     o[2 * i + 1] = t[i] >> 8; } }

(defun pack-25519 (o n)
  (let ((tmp (copy-seq n))
        (m (make-gf)))
    (dotimes (_ 3)
      (setf tmp (car-25519 tmp)))
    (dotimes (j 2)
      (setf (aref m 0) (- (aref tmp 0) #xFFED))
      (loop :for i :from 1 :below 15
        :do (setf (aref m i) (- (aref tmp i)
                                #xFFFF
                                (logand (ash (aref m (1- i)) -16)
                                        1))
                  (aref m (1- i)) (mask-field (byte 16 0) (aref m (1- i)))))
      (setf (aref m 15) (- (aref tmp 15)
                           #x7FFF
                           (logand (ash (aref m 14) -16)
                                   1)))
      (let ((b (mask-field (byte 1 0) (ash (aref m 15) -16))))
        (setf (aref m 15) (mask-field (byte 16 0) (aref m 15)))
        (sel-25519 tmp m (1- b))))
    (dotimes (i 16 o)
      (setf (aref o (* 2 i)) (mask-field (byte 8 0) (aref tmp i))
            (aref o (1+ (* 2 i))) (ash (aref tmp i) -8)))))

; static int neq25519 (const gf a, const gf b)
; { u8 c[32], d[32];
;   pack25519 (c, a);
;   pack25519 (d, b);
;   return crypto_verify_32 (c, d); }

(defun neq-25519 (a b)
  (let ((c (make-array 32 :element-type '-u8-))
        (d (make-array 32 :element-type '-u8-)))
    (setf c (pack-25519 c a)
          d (pack-25519 d b))
    (crypto-verify-32 c d)))

; static u8 par25519 (const gf a)
; { u8 d[32];
;   pack25519 (d, a);
;   return d[0] & 1; }

(defun par-25519 (a)
  (let ((d (make-array 32 :element-type '-u8-)))
    (setf d (pack-25519 d a))
    (mask-field (byte 1 0) (aref d 0))))

; sv unpack25519 (gf o, const u8 *n)
; { int i;
;   FOR (i, 16) o[i] = n[2 * i] + ((i64) n[2 * i + 1] << 8);
;   o[15] &= 0x7fff; }

(defun unpack-25519 (o n)
  (dotimes (i 16 o)
    (setf (aref o i) (+ (aref n (* 2 i))
                        (ash (aref n (1+ (* 2 i))) 8))))
  (setf (aref o 15) (mask-field (byte 15 0) (aref o 15)))
  o)

; sv A (gf o, const gf a, const gf b)
; { int i;
;   FOR (i, 16) o[i] = a[i] + b[i]; }

(defun a (o a b)
  (map-into o #'+ a b))

; sv Z (gf o, const gf a, const gf b)
; { int i;
;   FOR (i, 16) o[i] = a[i] - b[i]; }

(defun z (o a b)
  (map-into o #'- a b))

; sv M (gf o, const gf a, const gf b)
; { i64 i, j, t[31];
;   FOR (i, 31) t[i] = 0;
;   FOR (i, 16)
;     FOR (j, 16) t[i + j] += a[i] * b[j];
;   FOR (i, 15) t[i] += 38 * t[i + 16];
;   FOR (i, 16) o[i] = t[i];
;   car25519 (o);
;   car25519 (o); }

(defun m (o a b)
  (let ((tmp (make-array 31 :element-type '-i64- :initial-element 0)))
    (dotimes (i 16)
      (dotimes (j 16)
        (incf (aref tmp (+ i j)) (* (aref a i) (aref b i)))))
    (dotimes (i 15)
      (incf (aref tmp i) (* 38 (aref tmp (+ i 16)))))
    (map-into o #'identity tmp)
    (dotimes (i 2 o)
      (setf o (car-25519 o)))))

; sv S (gf o, const gf a)
; { M (o, a, a); }

(defun s (o a)
  (m o a a))

; sv inv25519 (gf o, const gf i)
; { gf c;
;   int a;
;   FOR (a, 16) c[a] = i[a];
;   for (a = 253; a >= 0; a--)
;   { S (c, c);
;     if (a != 2 && a != 4) M (c, c, i); }
;   FOR (a, 16) o[a] = c[a]; }

(defun inv-25519 (o i)
  (let ((c (copy-seq i)))
    (loop :for a :downfrom 253 :downto 0
      :do (setf c (s c c))
          (unless (member a '(2 4))
            (setf c (m c c i))))
    (map-into o #'identity c)))

; sv pow2523 (gf o, const gf i)
; { gf c;
;   int a;
;   FOR (a, 16) c[a] = i[a];
;   for (a = 250; a >= 0; a--)
;   { S (c, c);
;     if (a != 1) M (c, c, i); }
;   FOR (a, 16) o[a] = c[a]; }

(defun pow-2523 (o i)
  (let ((c (copy-seq i)))
    (loop :for a :downfrom 250 :downto 0
      :do (setf c (s c c))
          (when (/= a 1)
            (setf c (m c c i))))
    (map-into o #'identity c)))

; int crypto_scalarmult (u8 *q, const u8 *n, const u8 *p)
; { u8 z[32];
;   i64 x[96], r, i;
;   gf a, b, c, d, e, f;
;   FOR (i, 31) z[i] = n[i];
;   z[31] = (n[31] & 127) | 64;
;   z[0] &= 248;
;   unpack25519 (x, p);
;   FOR (i, 16)
;   { b[i] = x[i];
;     d[i] = a[i] = c[i] = 0; }
;   a[0] = d[0] = 1;
;   for (i = 254; i >= 0; --i)
;   { r = (z[i >> 3] >> (i & 7)) & 1;
;     sel25519 (a, b, r);
;     sel25519 (c, d, r);
;     A (e, a, c);
;     Z (a, a, c);
;     A (c, b, d);
;     Z (b, b, d);
;     S (d, e);
;     S (f, a);
;     M (a, c, a);
;     M (c, b, e);
;     A (e, a, c);
;     Z (a, a, c);
;     S (b, a);
;     Z (c, d, f);
;     M (a, c, _121665);
;     A (a, a, d);
;     M (c, c, a);
;     M (a, d, f);
;     M (d, b, x);
;     S (b, e);
;     sel25519 (a, b, r);
;     sel25519 (c, d, r); }
;   FOR (i, 16)
;   { x[i + 32] = a[i];
;     x[i + 48] = c[i];
;     x[i + 64] = b[i];
;     x[i + 80] = d[i]; }
;   inv25519 (x + 48, x + 48);
;   M (x + 32, x + 32, x + 48);
;   pack25519 (q, x + 32);
;   return 0; }

(defun crypto-scalarmult (q n p)
  (let ((z (make-array 32 :element-type '-u8-))
        (x (make-array 96 :element-type '-i64-))
        (a (make-gf-1))
        (b (make-gf))
        (c (make-gf))
        (d (make-gf-1))
        (e (make-gf))
        (f (make-gf)))
    (setf (subseq z 0 31) (subseq n 0 31)
          (aref z 31) (logior (mask-field (byte 7 0) (aref n 31))
                              64)
          (aref z 0) (mask-field (byte 5 3) (aref z 0))
          x (unpack-25519 x p)
          b (copy-seq x))
    (do* ((i 254 (1- i))
          (r (mask-field (byte 1 0)
                         (ash (aref z (ash i -3))
                              (- (mask-field (byte 3 0) i))))))
         ((minusp i))
      (sel-25519 a b r)
      (sel-25519 c d r)
      (setf e (a e a c)
            a (z a a c)
            c (a c b d)
            b (z b b d)
            d (s d e)
            f (s f a)
            a (m a c a)
            c (m c b e)
            e (a e a c)
            a (z a a c)
            b (s b a)
            c (z c d f)
            a (m a c *121665*)
            a (a a a d)
            c (m c c a)
            a (m a d f)
            d (m d b x)
            b (s b e))
      (sel-25519 a b r)
      (sel-25519 c d r))
    (dotimes (i 16)
      (setf (aref x (+ i 32)) (aref a i)
            (aref x (+ i 48)) (aref c i)
            (aref x (+ i 64)) (aref b i)
            (aref x (+ i 80)) (aref d i)))
    (setf (subseq x 48) (inv-25519 (subseq x 48) (subseq x 48))
          (subseq x 32) (m (subseq x 32) (subseq x 32) (subseq x 48))
          q (pack-25519 q (subseq x 32)))))

; int crypto_scalarmult_base (u8 *q, const u8 *n)
; { return crypto_scalarmult (q, n, _9); }

(defun crypto-scalarmult-base (q n)
  (crypto-scalarmult q n *9*))

; int crypto_box_keypair (u8 *y, u8 *x)
; { randombytes (x, 32);
;   return crypto_scalarmult_base (y, x); }

(defun crypto-box-keypair (y x)
  (setf x (randombytes x 32))
  (crypto-scalarmult-base y x))

; int crypto_box_beforenm (u8 *k, const u8 *y, const u8 *x)
; { u8 s[32];
;   crypto_scalarmult (s, x, y);
;   return crypto_core_hsalsa20 (k, _0, s, sigma); }

(defun crypto-box-beforenm (k y x
                            &aux
                              (s (make-array 32 :element-type '-u8-)))
  (crypto-scalarmult s x y)
  (crypto-core-hsalsa-20 k *0* s *sigma*))

; int crypto_box_afternm (u8 *c, const u8 *m, u64 d, const u8 *n, const u8 *k)
; { return crypto_secretbox (c, m, d, n, k); }

(defun crypto-box-afternm (c m d n k)
  (crypto-secretbox c m d n k))

; int crypto_box_open_afternm (u8 *m, const u8 *c, u64 d, const u8 *n, const u8 *k)
; { return crypto_secretbox_open (m, c, d, n, k); }

(defun crypto-box-open-afternm (m c d n k)
  (crypto-secretbox-open m c d n k))

; int crypto_box (u8 *c, const u8 *m, u64 d, const u8 *n, const u8 *y, const u8 *x)
; { u8 k[32];
;   crypto_box_beforenm (k, y, x);
;   return crypto_box_afternm (c, m, d, n, k); }

(defun crypto-box (c m d n y x
                   &aux
                     (k (make-array 32 :element-type '-u8-)))
  (crypto-box-beforenm k y x)
  (crypto-box-afternm c m d n k))

; int crypto_box_open (u8 *m, const u8 *c, u64 d, const u8 *n, const u8 *y, const u8 *x)
; { u8 k[32];
;   crypto_box_beforenm (k, y, x);
;   return crypto_box_open_afternm (m, c, d, n, k); }

(defun crypto-box-open (m c d n y x
                        &aux
                          (k (make-array 32 :element-type '-u8-)))
  (crypto-box-beforenm k y x)
  (crypto-box-open-afternm m c d n k))

; static u64 R (u64 x, int c)
; { return (x >> c) | (x << (64 - c)); }

(defun r (x c)
  (logior (ash x c)
          (ash x (- (- 64 c)))))

; static u64 Ch (u64 x, u64 y, u64 z)
; { return (x & y) ^ (~x & z); }

(defun ch (x y z)
  (logxor (logand x y)
          (logandc1 x z)))

; static u64 Maj (u64 x, u64 y, u64 z)
; { return (x & y) ^ (x & z) ^ (y & z); }

(defun maj (x y z)
  (logxor (logand x y)
          (logand x z)
          (logand y z)))

; static u64 Sigma0 (u64 x)
; { return R (x, 28) ^ R (x, 34) ^ R(x, 39); }

(defun sigma0 (x)
  (logxor (r x 28)
          (r x 34)
          (r x 39)))

; static u64 Sigma1 (u64 x)
; { return R (x, 14) ^ R (x, 18) ^ R (x, 41); }

(defun sigma1 (x)
  (logxor (r x 14)
          (r x 18)
          (r x 41)))

; static u64 sigma0 (u64 x)
; { return R (x, 1) ^ R (x, 8) ^ (x >> 7); }

(defun sigma0* (x)
  (logxor (r x 1)
          (r x 8)
          (ash x -7)))

; static u64 sigma1 (u64 x)
; { return R (x, 19) ^ R (x, 61) ^ (x >> 6); }

(defun sigma1* (x)
  (logxor (r x 19)
          (r x 61)
          (ash x -6)))

; static const u64 K[80] = {0x428a2f98d728ae22ULL, 0x7137449123ef65cdULL,
;                           0xb5c0fbcfec4d3b2fULL, 0xe9b5dba58189dbbcULL,
;                           0x3956c25bf348b538ULL, 0x59f111f1b605d019ULL,
;                           0x923f82a4af194f9bULL, 0xab1c5ed5da6d8118ULL,
;                           0xd807aa98a3030242ULL, 0x12835b0145706fbeULL,
;                           0x243185be4ee4b28cULL, 0x550c7dc3d5ffb4e2ULL,
;                           0x72be5d74f27b896fULL, 0x80deb1fe3b1696b1ULL,
;                           0x9bdc06a725c71235ULL, 0xc19bf174cf692694ULL,
;                           0xe49b69c19ef14ad2ULL, 0xefbe4786384f25e3ULL,
;                           0x0fc19dc68b8cd5b5ULL, 0x240ca1cc77ac9c65ULL,
;                           0x2de92c6f592b0275ULL, 0x4a7484aa6ea6e483ULL,
;                           0x5cb0a9dcbd41fbd4ULL, 0x76f988da831153b5ULL,
;                           0x983e5152ee66dfabULL, 0xa831c66d2db43210ULL,
;                           0xb00327c898fb213fULL, 0xbf597fc7beef0ee4ULL,
;                           0xc6e00bf33da88fc2ULL, 0xd5a79147930aa725ULL,
;                           0x06ca6351e003826fULL, 0x142929670a0e6e70ULL,
;                           0x27b70a8546d22ffcULL, 0x2e1b21385c26c926ULL,
;                           0x4d2c6dfc5ac42aedULL, 0x53380d139d95b3dfULL,
;                           0x650a73548baf63deULL, 0x766a0abb3c77b2a8ULL,
;                           0x81c2c92e47edaee6ULL, 0x92722c851482353bULL,
;                           0xa2bfe8a14cf10364ULL, 0xa81a664bbc423001ULL,
;                           0xc24b8b70d0f89791ULL, 0xc76c51a30654be30ULL,
;                           0xd192e819d6ef5218ULL, 0xd69906245565a910ULL,
;                           0xf40e35855771202aULL, 0x106aa07032bbd1b8ULL,
;                           0x19a4c116b8d2d0c8ULL, 0x1e376c085141ab53ULL,
;                           0x2748774cdf8eeb99ULL, 0x34b0bcb5e19b48a8ULL,
;                           0x391c0cb3c5c95a63ULL, 0x4ed8aa4ae3418acbULL,
;                           0x5b9cca4f7763e373ULL, 0x682e6ff3d6b2b8a3ULL,
;                           0x748f82ee5defb2fcULL, 0x78a5636f43172f60ULL,
;                           0x84c87814a1f0ab72ULL, 0x8cc702081a6439ecULL,
;                           0x90befffa23631e28ULL, 0xa4506cebde82bde9ULL,
;                           0xbef9a3f7b2c67915ULL, 0xc67178f2e372532bULL,
;                           0xca273eceea26619cULL, 0xd186b8c721c0c207ULL,
;                           0xeada7dd6cde0eb1eULL, 0xf57d4f7fee6ed178ULL,
;                           0x06f067aa72176fbaULL, 0x0a637dc5a2c898a6ULL,
;                           0x113f9804bef90daeULL, 0x1b710b35131c471bULL,
;                           0x28db77f523047d84ULL, 0x32caab7b40c72493ULL,
;                           0x3c9ebe0a15c9bebcULL, 0x431d67c49c100d4cULL,
;                           0x4cc5d4becb3e42b6ULL, 0x597f299cfc657e2aULL,
;                           0x5fcb6fab3ad6faecULL, 0x6c44198c4a475817ULL};

(defparameter *k* #(#x428a2f98d728ae22 #x7137449123ef65cd #xb5c0fbcfec4d3b2f
                    #xe9b5dba58189dbbc #x3956c25bf348b538 #x59f111f1b605d019
                    #x923f82a4af194f9b #xab1c5ed5da6d8118 #xd807aa98a3030242
                    #x12835b0145706fbe #x243185be4ee4b28c #x550c7dc3d5ffb4e2
                    #x72be5d74f27b896f #x80deb1fe3b1696b1 #x9bdc06a725c71235
                    #xc19bf174cf692694 #xe49b69c19ef14ad2 #xefbe4786384f25e3
                    #x0fc19dc68b8cd5b5 #x240ca1cc77ac9c65 #x2de92c6f592b0275
                    #x4a7484aa6ea6e483 #x5cb0a9dcbd41fbd4 #x76f988da831153b5
                    #x983e5152ee66dfab #xa831c66d2db43210 #xb00327c898fb213f
                    #xbf597fc7beef0ee4 #xc6e00bf33da88fc2 #xd5a79147930aa725
                    #x06ca6351e003826f #x142929670a0e6e70 #x27b70a8546d22ffc
                    #x2e1b21385c26c926 #x4d2c6dfc5ac42aed #x53380d139d95b3df
                    #x650a73548baf63de #x766a0abb3c77b2a8 #x81c2c92e47edaee6
                    #x92722c851482353b #xa2bfe8a14cf10364 #xa81a664bbc423001
                    #xc24b8b70d0f89791 #xc76c51a30654be30 #xd192e819d6ef5218
                    #xd69906245565a910 #xf40e35855771202a #x106aa07032bbd1b8
                    #x19a4c116b8d2d0c8 #x1e376c085141ab53 #x2748774cdf8eeb99
                    #x34b0bcb5e19b48a8 #x391c0cb3c5c95a63 #x4ed8aa4ae3418acb
                    #x5b9cca4f7763e373 #x682e6ff3d6b2b8a3 #x748f82ee5defb2fc
                    #x78a5636f43172f60 #x84c87814a1f0ab72 #x8cc702081a6439ec
                    #x90befffa23631e28 #xa4506cebde82bde9 #xbef9a3f7b2c67915
                    #xc67178f2e372532b #xca273eceea26619c #xd186b8c721c0c207
                    #xeada7dd6cde0eb1e #xf57d4f7fee6ed178 #x06f067aa72176fba
                    #x0a637dc5a2c898a6 #x113f9804bef90dae #x1b710b35131c471b
                    #x28db77f523047d84 #x32caab7b40c72493 #x3c9ebe0a15c9bebc
                    #x431d67c49c100d4c #x4cc5d4becb3e42b6 #x597f299cfc657e2a
                    #x5fcb6fab3ad6faec #x6c44198c4a475817))

; int crypto_hashblocks (u8 *x, const u8 *m, u64 n)
; { u64 z[8], b[8], a[8], w[16], t;
;   int i, j;
;   FOR (i, 8) z[i] = a[i] = dl64 (x + 8 * i);
;   while (n >= 128)
;   { FOR (i, 16) w[i] = dl64 (m + 8 * i);
;     FOR (i,80)
;     { FOR (j, 8) b[j] = a[j];
;       t = a[7] + Sigma1 (a[4]) + Ch (a[4], a[5], a[6]) + K[i] + w[i % 16];
;       b[7] = t + Sigma0 (a[0]) + Maj (a[0], a[1], a[2]);
;       b[3] += t;
;       FOR (j, 8) a[(j + 1) % 8] = b[j];
;       if (i % 16 == 15)
;         FOR (j, 16)
;           w[j] += w[(j + 9) % 16] +
;                   sigma0 (w[(j + 1) % 16]) +
;                   sigma1 (w[(j + 14) % 16]); }
;     FOR (i, 8)
;     { a[i] += z[i];
;       z[i] = a[i]; }
;     m += 128;
;     n -= 128; }
;   FOR (i, 8) ts64 (x + 8 * i, z[i]);
;   return n; }

(defun crypto-hashblocks (x m n
                          &aux
                            (z (make-array 8 :element-type '-u64-))
                            (b (make-array 8 :element-type '-u64-))
                            (a (make-array 8 :element-type '-u64-))
                            (w (make-array 16 :element-type '-u64-)))
  (dotimes (i 8)
    (let ((_ (dl-64 (subseq x (* 8 i)))))
      (setf (aref a i) _
            (aref z i) _)))
  (do () ((< n 128))
    (dotimes (i 16)
      (setf (aref w i) (dl-64 (subseq m (* 8 i)))))
    (dotimes (i 80)
      (map-into b #'identity a)
      (let ((tmp (+ (aref a 7)
                    (sigma1 (aref a 4))
                    (ch (aref a 4) (aref a 5) (aref a 6))
                    (aref *k* i)
                    (aref w (mod i 16)))))
        (setf (aref b 7) (+ tmp
                            (sigma0 (aref a 0))
                            (maj (aref a 0) (aref a 1) (aref a 2))))
        (incf (aref b 3) tmp)
        (dotimes (j 8)
          (setf (aref a (mod (1+ j) 8)) (aref b j)))
        (when (= (mod i 16) 15)
          (dotimes (j 16)
            (incf (aref w j) (+ (aref w (mod (+ j 9) 16))
                                (sigma0* (aref w (mod (+ j 1) 16)))
                                (sigma1* (aref w (mod (+ j 14) 16)))))))))
    (dotimes (i 8)
      (incf (aref a i) (aref z i))
      (setf (aref z i) (aref a i)))
    (incf m 128)
    (decf n 128))
  (dotimes (i 8)
    (setf (subseq x (* 8 i) (* 8 (1+ i))) (ts-64 (aref z i))))
  n)

; static const u8 iv[64] = {0x6a, 0x09, 0xe6, 0x67, 0xf3, 0xbc, 0xc9, 0x08,
;                           0xbb, 0x67, 0xae, 0x85, 0x84, 0xca, 0xa7, 0x3b,
;                           0x3c, 0x6e, 0xf3, 0x72, 0xfe, 0x94, 0xf8, 0x2b,
;                           0xa5, 0x4f, 0xf5, 0x3a, 0x5f, 0x1d, 0x36, 0xf1,
;                           0x51, 0x0e, 0x52, 0x7f, 0xad, 0xe6, 0x82, 0xd1,
;                           0x9b, 0x05, 0x68, 0x8c, 0x2b, 0x3e, 0x6c, 0x1f,
;                           0x1f, 0x83, 0xd9, 0xab, 0xfb, 0x41, 0xbd, 0x6b,
;                           0x5b, 0xe0, 0xcd, 0x19, 0x13, 0x7e, 0x21, 0x79};

(defparameter *iv* #(#x6a #x09 #xe6 #x67 #xf3 #xbc #xc9 #x08
                     #xbb #x67 #xae #x85 #x84 #xca #xa7 #x3b
                     #x3c #x6e #xf3 #x72 #xfe #x94 #xf8 #x2b
                     #xa5 #x4f #xf5 #x3a #x5f #x1d #x36 #xf1
                     #x51 #x0e #x52 #x7f #xad #xe6 #x82 #xd1
                     #x9b #x05 #x68 #x8c #x2b #x3e #x6c #x1f
                     #x1f #x83 #xd9 #xab #xfb #x41 #xbd #x6b
                     #x5b #xe0 #xcd #x19 #x13 #x7e #x21 #x79))

; int crypto_hash (u8*out, const u8*m, u64 n)
; { u8 h[64], x[256];
;   u64 i, b = n;
;   FOR (i, 64) h[i] = iv[i];
;   crypto_hashblocks (h, m, n);
;   m += n;
;   n &= 127;
;   m -= n;
;   FOR (i, 256) x[i] = 0;
;   FOR (i, n) x[i] = m[i];
;   x[n] = 128;
;   n = 256 - 128 * (n < 112);
;   x[n - 9] = b >> 61;
;   ts64 (x + n - 8, b << 3);
;   crypto_hashblocks (h, x, n);
;   FOR (i, 64) out[i] = h[i];
;   return 0; }

(defun crypto-hash (out m n
                    &aux
                      (h (copy-seq *iv*))
                      (x (make-array 256 :element-type '-u8- :initial-element 0))
                      (b n))
  (crypto-hashblocks h m n)
  (setf n (mask-field (byte 7 0) n)
        m (subseq m (- b n))
        (subseq x 0 n) (subseq m 0 n)
        (aref x n) 128
        n (- 256 (* 128 (if (< n 112) 1 0)))
        (aref x (- n 9)) (ash b -61)
        (subseq x (- n 8) n) (ts-64 (ash b 3)))
  (crypto-hashblocks h x n)
  (setf (subseq out 0 64) h)
  out)

;sv add(gf p[4], gf q[4]){ gf a, b, c, d, t, e, f, g, h;
; Z(a, p[1], p[0]);
; Z(t, q[1], q[0]);
; M(a, a, t);
; A(b, p[0], p[1]);
; A(t, q[0], q[1]);
; M(b, b, t);
; M(c, p[3], q[3]);
; M(c, c, D2);
; M(d, p[2], q[2]);
; A(d, d, d);
; Z(e, b, a);
; Z(f, d, c);
; A(g, d, c);
; A(h, b, a);
; M(p[0], e, f);
; M(p[1], h, g);
; M(p[2], g, f);
; M(p[3], e, h); }

(defun add (p q
            &aux
              (a (make-gf))
              (b (make-gf))
              (c (make-gf))
              (d (make-gf))
              (tmp (make-gf))
              (e (make-gf))
              (f (make-gf))
              (g (make-gf))
              (h (make-gf)))
  (setf a (z a (aref p 1) (aref p 0))
        tmp (z tmp (aref q 1) (aref q 0))
        a (m a a tmp)
        b (a b (aref p 0) (aref p 1))
        tmp (a tmp (aref q 0) (aref q 1))
        b (m b b tmp)
        c (m c (aref p 3) (aref q 3))
        c (m c c *d2*)
        d (m d (aref p 2) (aref q 2))
        d (a d d d)
        e (z e b a)
        f (z f d c)
        g (a g d c)
        h (a h b a)
        (aref p 0) (m (aref p 0) e f)
        (aref p 1) (m (aref p 1) h g)
        (aref p 2) (m (aref p 2) g f)
        (aref p 3) (m (aref p 3) e h))
  p)

; sv cswap (gf p[4], gf q[4], u8 b)
; { int i;
;   FOR (i, 4) sel25519 (p[i], q[i], b); }

(defun cswap (p q b)
  (dotimes (i 4)
    (sel-25519 (aref p i) (aref q i) b)))

; sv pack (u8 *r, gf p[4])
; { gf tx, ty, zi;
;   inv25519 (zi, p[2]);
;   M (tx, p[0], zi);
;   M (ty, p[1], zi);
;   pack25519 (r, ty);
;   r[31] ^= par25519 (tx) << 7; }

(defun pack (r p
             &aux
               (tx (make-gf))
               (ty (make-gf))
               (zi (make-gf)))
  (setf zi (inv-25519 zi (aref p 2))
        tx (m tx (aref p 0) zi)
        ty (m ty (aref p 1) zi)
        r (pack-25519 r ty)
        (aref r 31) (logxor (aref r 31)
                            (ash (par-25519 tx) 7)))
  r)

; sv scalarmult (gf p[4], gf q[4], const u8 *s)
; { int i;
;   set25519 (p[0], gf0);
;   set25519 (p[1], gf1);
;   set25519 (p[2], gf1);
;   set25519 (p[3], gf0);
;   for (i = 255; i >= 0; --i)
;   { u8 b = (s[i/8] >> (i & 7)) & 1;
;     cswap (p, q, b);
;     add (q, p);
;     add (p, p);
;     cswap (p, q, b); } }

(defun scalarmult (p q s)
  (set-25519 (aref p 0) *gf0*)
  (set-25519 (aref p 1) *gf1*)
  (set-25519 (aref p 2) *gf1*)
  (set-25519 (aref p 3) *gf0*)
  (loop
    :for i :downfrom 255 :downto 0
    :for b := (mask-field (byte 1 0)
                          (ash (aref s (floor i 8))
                               (- (mask-field (byte 3 0) i))))
    :do (cswap p q b)
        (add q p)
        (add p p)
        (cswap p q b)))

; sv scalarbase (gf p[4], const u8 *s)
; { gf q[4];
;   set25519 (q[0], X);
;   set25519 (q[1], Y);
;   set25519 (q[2], gf1);
;   M (q[3], X, Y);
;   scalarmult (p, q, s); }

(defun scalarbase (p s
                   &aux (q (make-array 4
                                       :initial-contents
                                       (loop :repeat 4
                                         :collect (make-gf)))))
  (set-25519 (aref q 0) *x*)
  (set-25519 (aref q 1) *y*)
  (set-25519 (aref q 2) *gf1*)
  (setf (aref q 3) (m (aref q 3) *x* *y*))
  (scalarmult p q s))

;int crypto_sign_keypair (u8 *pk, u8 *sk)
; { u8 d[64];
;   gf p[4];
;   int i;
;   randombytes (sk, 32);
;   crypto_hash (d, sk, 32);
;   d[0] &= 248;
;   d[31] &= 127;
;   d[31] |= 64;
;   scalarbase (p, d);
;   pack (pk, p);
;   FOR (i, 32) sk[32 + i] = pk[i];
;   return 0; }

(defun crypto-sign-keypair (pk sk
                            &aux
                              (d (make-array 64 :element-type '-u8-))
                              (p (make-array 4
                                             :initial-contents
                                             (loop :repeat 4
                                               :collect (make-gf)))))
  (randombytes 32 sk)
  (crypto-hash d sk 32)
  (setf (aref d 0) (mask-field (byte 5 3) (aref d 0))
        (aref d 31) (mask-field (byte 7 0) (aref d 31))
        (aref d 31) (logior (aref d 31) 64))
  (scalarbase p d)
  (pack pk p)
  (setf (subseq sk 32 64) pk))

; static const u64 L[32] = {0xed, 0xd3, 0xf5, 0x5c, 0x1a, 0x63, 0x12, 0x58,
;                           0xd6, 0x9c, 0xf7, 0xa2, 0xde, 0xf9, 0xde, 0x14,
;                           0, 0, 0, 0, 0, 0, 0, 0,
;                           0, 0, 0, 0, 0, 0, 0, 0x10};

(defparameter *l* #(#xed #xd3 #xf5 #x5c #x1a #x63 #x12 #x58
                    #xd6 #x9c #xf7 #xa2 #xde #xf9 #xde #x14
                    0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 #x10))

; sv modL (u8*r, i64 x[64])
; { i64 carry, i, j;
;   for (i = 63; i >= 32; --i)
;   { carry = 0;
;     for (j = i - 32; j < i - 12; ++j)
;     { x[j] += carry - 16 * x[i] * L[j - (i - 32)];
;       carry = (x[j] + 128) >> 8;
;       x[j] -= carry << 8; }
;     x[j] += carry;
;     x[i] = 0; }
;   carry = 0;
;   FOR (j, 32)
;   { x[j] += carry - (x[31] >> 4) * L[j];
;     carry = x[j] >> 8;
;     x[j] &= 255; }
;   FOR (j, 32) x[j] -= carry * L[j];
;   FOR (i, 32)
;   { x[i + 1] += x[i] >> 8;
;     r[i] = x[i] & 255; } }

(defun mod-l (r x)
  (loop
    :for i :downfrom 63 :downto 32
    :for carry := 0
    :do (loop :for j :from (- i 32) :below (- i 12)
          :do (incf (aref x j) (- carry
                                  (* 16
                                     (aref x i)
                                     (aref *l* (- j (- i 32))))))
              (setf carry (ash (+ (aref x j) 128) -8))
              (decf (aref x j) (ash carry 8)))
        (incf (aref x (- i 12)) carry)
        (setf (aref x i) 0))
  (loop :with carry := 0
    :for j :below 32
    :do (incf (aref x j) (- carry (* (ash (aref x 31) -4)
                                     (aref *l* j))))
        (setf carry (ash (aref x j) -8)
              (aref x j) (mask-field (byte 8 0) (aref x j)))
    :finally
        (dotimes (j 32)
          (decf (aref x j) (* carry (aref *l* j)))))
  (dotimes (i 32 r)
    (incf (aref x (1+ i)) (ash (aref x i) -8))
    (setf (aref r i) (mask-field (byte 8 0) (aref x i)))))

; sv reduce (u8 *r)
; { i64 x[64], i;
;   FOR (i, 64) x[i] = (u64) r[i];
;   FOR (i, 64) r[i] = 0;
;   modL (r, x); }

(defun reduce* (r &aux (x (make-array 64 :element-type '-i64-)))
  (dotimes (i 64)
    (shiftf (aref x i) (aref r i) 0))
  (setf r (mod-l r x)))

; int crypto_sign (u8 *sm, u64 *smlen, const u8 *m, u64 n, const u8 *sk)
; { u8 d[64], h[64], r[64];
;   i64 i, j, x[64];
;   gf p[4];
;   crypto_hash (d, sk, 32);
;   d[0] &= 248;
;   d[31] &= 127;
;   d[31] |= 64;
;   *smlen = n + 64;
;   FOR (i, n) sm[64 + i] = m[i];
;   FOR (i, 32) sm[32 + i] = d[32 + i];
;   crypto_hash (r, sm + 32, n + 32);
;   reduce (r);
;   scalarbase (p, r);
;   pack (sm, p);
;   FOR (i, 32) sm[i + 32] = sk[i + 32];
;   crypto_hash (h, sm, n + 64);
;   reduce (h);
;   FOR (i, 64) x[i] = 0;
;   FOR (i, 32) x[i] = (u64) r[i];
;   FOR (i, 32) FOR (j, 32) x[i + j] += h[i] * (u64) d[j];
;   modL (sm + 32, x);
;   return 0; }

(defun crypto-sign (sm smlen m n sk
                    &aux
                      (d (make-array 64 :element-type '-u8-))
                      (h (make-array 64 :element-type '-u8-))
                      (r (make-array 64 :element-type '-u8-))
                      (x (make-array 64
                                     :element-type '-i64-
                                     :initial-element 0))
                      (p (make-array 4
                                     :initial-contents (loop :repeat 4
                                                         :collect (make-gf)))))
  (setf d (crypto-hash d sk 32)
        (aref d 0) (mask-field (byte 5 3) (aref d 0))
        (aref d 31) (mask-field (byte 7 0) (aref d 31))
        (aref d 31) (logand (aref d 31) 64)
        (subseq sm 64 (+ 64 n)) (subseq m 0 n)
        (subseq sm 32 64) (subseq d 32 64)
        r (crypto-hash r (subseq sm 32) (+ n 32))
        r (reduce* r))
  (scalarbase p r)
  (pack sm p)
  (setf (subseq sm 32 64) (subseq sk 32 64)
        h (crypto-hash h sm (+ n 64))
        h (reduce* h))
  (dotimes (i 64)
    (setf (aref x i) 0))
  (setf (subseq x 0 32) (subseq r 0 32))
  (dotimes (i 32)
    (dotimes (j 32)
      (incf (aref x (+ i j)) (* (aref h i) (aref d j)))))
  (mod-l (sub-vector sm 32 64) x)
  (values sm smlen))

; static int unpackneg (gf r[4], const u8 p[32])
; { gf t, chk, num, den, den2, den4, den6;
;   set25519 (r[2], gf1);
;   unpack25519 (r[1], p);
;   S (num, r[1]);
;   M (den, num, D);
;   Z (num, num, r[2]);
;   A (den, r[2], den);
;   S (den2, den);
;   S (den4, den2);
;   M (den6, den4, den2);
;   M (t, den6, num);
;   M (t, t, den);
;   pow2523 (t, t);
;   M (t, t, num);
;   M (t, t, den);
;   M (t, t, den);
;   M (r[0], t, den);
;   S (chk, r[0]);
;   M (chk, chk, den);
;   if (neq25519 (chk, num)) M (r[0], r[0], I);
;   S (chk, r[0]);
;   M (chk, chk, den);
;   if (neq25519 (chk, num)) return -1;
;   if (par25519 (r[0]) == (p[31]>>7)) Z (r[0], gf0, r[0]);
;   M (r[3], r[0], r[1]);
;   return 0; }

(defmacro with-gfs ((&rest vars) &body body)
  `(let (,@(mapcar (lambda (var)
                     `(,var (make-gf)))
                   vars))
     ,@body))

(defun unpackneg (r p)
  (with-gfs (tmp chk num den den2 den4 den6)
    (set-25519 (aref r 2) *gf1*)
    (unpack-25519 (aref r 1) p)
    (setf num (s num (aref r 1))
          den (m den num *d*)
          num (z num num (aref r 2))
          den (a den (aref r 2) den)
          den2 (s den2 den)
          den4 (s den4 den2)
          den6 (m den6 den4 den2)
          tmp (m tmp den6 num)
          tmp (m tmp tmp den)
          tmp (pow-2523 tmp tmp)
          tmp (m tmp tmp num)
          tmp (m tmp tmp den)
          tmp (m tmp tmp den)
          (aref r 0) (m (aref r 0) tmp den)
          chk (s chk (aref r 0))
          chk (m chk chk den))
    (when (neq-25519 chk num)
      (setf (aref r 0) (m (aref r 0) (aref r 0) *i*)))
    (setf chk (s chk (aref r 0))
          chk (m chk chk den))
    (when (neq-25519 chk num)
      (return-from unpackneg :check-fail))
    (when (= (par-25519 (aref r 0))
             (ash (aref p 31) -7))
      (setf (aref r 0) (z (aref r 0) *gf0* (aref r 0))))
    (setf (aref r 3) (m (aref r 3) (aref r 0) (aref r 1)))
    r))

; int crypto_sign_open (u8 *m, u64 *mlen, const u8 *sm, u64 n, const u8 *pk)
; { int i;
;   u8 t[32], h[64];
;   gf p[4], q[4];
;   *mlen = -1;
;   if (n < 64) return -1;
;   if (unpackneg (q, pk)) return -1;
;   FOR (i, n) m[i] = sm[i];
;   FOR (i, 32) m[i+32] = pk[i];
;   crypto_hash (h, m, n);
;   reduce (h);
;   scalarmult (p, q, h);
;   scalarbase (q, sm + 32);
;   add (p, q);
;   pack (t, p);
;   n -= 64;
;   if (crypto_verify_32 (sm, t))
;   { FOR (i, n) m[i] = 0;
;     return -1; }
;   FOR (i, n) m[i] = sm[i + 64];
;   *mlen = n;
;   return 0; }

(defun make-4-gf-array ()
  (make-array 4
              :initial-contents
              (loop :repeat 4
                :collect (make-gf))))

(defun crypto-sign-open (m sm n pk
                         &aux
                           (tmp (make-array 32 :element-type '-u8-))
                           (h (make-array 64 :element-type '-u8-))
                           (p (make-4-gf-array))
                           (q (make-4-gf-array)))
  (cond ((< n 64) (error "N is too small"))
        ((eq (unpackneg q pk) :check-fail) (error "Check failed")))
  (setf (subseq m 0 n) sm
        (subseq m 32 64) pk
        h (crypto-hash h m n)
        h (reduce* h))
  (scalarmult p q h)
  (scalarbase q (sub-vector sm 32 (length sm)))
  (setf p (add p q)
        tmp (pack tmp p))
  (decf n 64)
  (if (crypto-verify-32 sm tmp)
      (progn
        (map-into (sub-vector m 0 n) (constantly 0))
        (values t m -1))
      (progn
        (setf (subseq m 0 n) (subseq sm 64))
        (values nil m n))))
