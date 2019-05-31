#include "precompiled.hpp"
#include "libadt/set.hpp"
#include "memory/allocation.inline.hpp"

// Sets - An Abstract Data Type

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

//-------------------------Virtual Functions-----------------------------------
// These functions MUST be implemented by the inheriting class.
class SparseSet;
const SparseSet *Set::asSparseSet() const {
    return NULL; }
class VectorSet;
const VectorSet *Set::asVectorSet() const {
    return NULL; }
class ListSet;
const ListSet *Set::asListSet() const {
    return NULL; }
class CoSet;
const CoSet *Set::asCoSet() const {
    return NULL; }

//------------------------------setstr-----------------------------------------
// Create a string with a printable representation of a set.
// The caller must deallocate the string.
char *Set::setstr() const
{
  if (this == NULL ) return os::strdup("{no set}");
  Set &set = clone();           // Virtually copy the basic set.
  set.Sort();                   // Sort elements for in-order retrieval

  uint len = 128;               // Total string space
  char *buf = NEW_C_HEAP_ARRAY(char,len, mtCompiler);// Some initial string space

  register char *s = buf;       // Current working string pointer
  *s++ = '{';
  *s = '\0';

  // For all elements of the Set
  uint hi = (uint)-2, lo = (uint)-2;
  for ( SetI i(&set); i.test(); ++i ) {
    if (hi+1 == i.elem ) {        // Moving sequentially thru range?
      hi = i.elem;                // Yes, just update hi end of range
    } else {                      // Else range ended
      if (buf+len-s < 25 ) {      // Generous trailing space for upcoming numbers
        int offset = (int)(s-buf);// Not enuf space; compute offset into buffer
        len <<= 1;                // Double string size
        buf = REALLOC_C_HEAP_ARRAY(char,buf,len, mtCompiler); // Reallocate doubled size
        s = buf+offset;         // Get working pointer into new bigger buffer
      }
      if (lo != (uint)-2 ) {    // Startup?  No!  Then print previous range.
        if (lo != hi ) sprintf(s,"%d-%d,",lo,hi);
        else sprintf(s,"%d,",lo);
        s += strlen(s);         // Advance working string
      }
      hi = lo = i.elem;
    }
  }
  if (lo != (uint)-2 ) {
    if (buf+len-s < 25 ) {      // Generous trailing space for upcoming numbers
      int offset = (int)(s-buf);// Not enuf space; compute offset into buffer
      len <<= 1;                // Double string size
      buf = (char*)ReallocateHeap(buf,len, mtCompiler); // Reallocate doubled size
      s = buf+offset;           // Get working pointer into new bigger buffer
    }
    if (lo != hi ) sprintf(s,"%d-%d}",lo,hi);
    else sprintf(s,"%d}",lo);
  } else strcat(s,"}");
  // Don't delete the clone 'set' since it is allocated on Arena.
  return buf;
}

//------------------------------print------------------------------------------
// Handier print routine
void Set::print() const
{
  char *printable_set = setstr();
  tty->print_cr("%s", printable_set);
  FreeHeap(printable_set);
}

//------------------------------parse------------------------------------------
// Convert a textual representation of a Set, to a Set and union into "this"
// Set.  Return the amount of text parsed in "len", or zero in "len".
int Set::parse(const char *s)
{
  register char c;              // Parse character
  register const char *t = s;   // Save the starting position of s.
  do c = *s++;                  // Skip characters
  while ( c && (c <= ' '));     // Till no more whitespace or EOS
  if (c != '{' ) return 0;      // Oops, not a Set openner
  if (*s == '}' ) return 2;     // The empty Set

  // Sets are filled with values of the form "xx," or "xx-yy," with the comma
  // a "}" at the very end.
  while ( 1 ) {                  // While have elements in the Set
    char *u;                    // Pointer to character ending parse
    uint hi, i;                 // Needed for range handling below
    uint elem = (uint)strtoul(s,&u,10);// Get element
    if (u == s ) return 0;      // Bogus crude
    s = u;                      // Skip over the number
    c = *s++;                   // Get the number seperator
    switch ( c ) {              // Different seperators
    case '}':                   // Last simple element
    case ',':                   // Simple element
      (*this) <<= elem;         // Insert the simple element into the Set
      break;                    // Go get next element
    case '-':                   // Range
      hi = (uint)strtoul(s,&u,10); // Get element
      if (u == s ) return 0;    // Bogus crude
      for ( i=elem; i<=hi; i++ )
        (*this) <<= i;          // Insert the entire range into the Set
      s = u;                    // Skip over the number
      c = *s++;                 // Get the number seperator
      break;
    }
    if (c == '}' ) break;       // End of the Set
    if (c != ',' ) return 0;    // Bogus garbage
  }
  return (int)(s-t);            // Return length parsed
}

//------------------------------Iterator---------------------------------------
SetI_::~SetI_()
{
}
