/* __mp_bases -- Structure for conversion between internal binary
   format and strings in base 2..36.  The fields are explained in
   gmp-impl.h.

   ***** THIS FILE WAS CREATED BY A PROGRAM.  DON'T EDIT IT! *****

Copyright (C) 1991 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2, or
(at your option) any later version.

The GNU MP Library is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU MP Library; see the file COPYING.  If not, write
to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
USA.  */

#include "gmp.h"
#include "gmp-impl.h"

const struct bases __mp_bases[37] =
{
  /*  0 */ {0, 0, 0, 0.0},
  /*  1 */ {0, 0, 0, 0.0},
  /*  2 */ {32, 0x1, 0x0, 1.00000000},
  /*  3 */ {20, 0xCFD41B91, 0x3B563C24, 0.63092975},
  /*  4 */ {16, 0x2, 0x0, 0.50000000},
  /*  5 */ {13, 0x48C27395, 0xC25C2684, 0.43067656},
  /*  6 */ {12, 0x81BF1000, 0xF91BD1B6, 0.38685281},
  /*  7 */ {11, 0x75DB9C97, 0x1607A2CB, 0.35620719},
  /*  8 */ {10, 0x3, 0x0, 0.33333333},
  /*  9 */ {10, 0xCFD41B91, 0x3B563C24, 0.31546488},
  /* 10 */ {9, 0x3B9ACA00, 0x12E0BE82, 0.30103000},
  /* 11 */ {9, 0x8C8B6D2B, 0xD24CDE04, 0.28906483},
  /* 12 */ {8, 0x19A10000, 0x3FA39AB5, 0.27894295},
  /* 13 */ {8, 0x309F1021, 0x50F8AC5F, 0.27023815},
  /* 14 */ {8, 0x57F6C100, 0x74843B1E, 0.26264954},
  /* 15 */ {8, 0x98C29B81, 0xAD0326C2, 0.25595802},
  /* 16 */ {8, 0x4, 0x0, 0.25000000},
  /* 17 */ {7, 0x18754571, 0x4EF0B6BD, 0.24465054},
  /* 18 */ {7, 0x247DBC80, 0xC0FC48A1, 0.23981247},
  /* 19 */ {7, 0x3547667B, 0x33838942, 0.23540891},
  /* 20 */ {7, 0x4C4B4000, 0xAD7F29AB, 0.23137821},
  /* 21 */ {7, 0x6B5A6E1D, 0x313C3D15, 0.22767025},
  /* 22 */ {7, 0x94ACE180, 0xB8CCA9E0, 0.22424382},
  /* 23 */ {7, 0xCAF18367, 0x42ED6DE9, 0.22106473},
  /* 24 */ {6, 0xB640000, 0x67980E0B, 0.21810429},
  /* 25 */ {6, 0xE8D4A51, 0x19799812, 0.21533828},
  /* 26 */ {6, 0x1269AE40, 0xBCE85396, 0.21274605},
  /* 27 */ {6, 0x17179149, 0x62C103A9, 0.21030992},
  /* 28 */ {6, 0x1CB91000, 0x1D353D43, 0.20801460},
  /* 29 */ {6, 0x23744899, 0xCE1DECEA, 0.20584683},
  /* 30 */ {6, 0x2B73A840, 0x790FC511, 0.20379505},
  /* 31 */ {6, 0x34E63B41, 0x35B865A0, 0.20184909},
  /* 32 */ {6, 0x5, 0x0, 0.20000000},
  /* 33 */ {6, 0x4CFA3CC1, 0xA9AED1B3, 0.19823986},
  /* 34 */ {6, 0x5C13D840, 0x63DFC229, 0.19656163},
  /* 35 */ {6, 0x6D91B519, 0x2B0FEE30, 0.19495902},
  /* 36 */ {6, 0x81BF1000, 0xF91BD1B6, 0.19342640},
};
