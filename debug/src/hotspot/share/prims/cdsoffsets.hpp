#ifndef SHARE_PRIMS_CDSOFFSETS_HPP
#define SHARE_PRIMS_CDSOFFSETS_HPP

class CDSOffsets: public CHeapObj<mtInternal> {
 private:
  char* _name;
  int   _offset;
  CDSOffsets* _next;
  static CDSOffsets* _all;  // sole list for cds
 public:
  CDSOffsets(const char* name, int offset, CDSOffsets* next);

  char* get_name() const { return _name; }
  int   get_offset() const { return _offset; }
  CDSOffsets* next() const { return _next; }
  void add_end(CDSOffsets* n);

  static int find_offset(const char* name);
};

#endif
