#ifndef SHARE_VM_MEMORY_METASPACECHUNKFREELISTSUMMARY_HPP
#define SHARE_VM_MEMORY_METASPACECHUNKFREELISTSUMMARY_HPP

class MetaspaceChunkFreeListSummary {
  size_t _num_specialized_chunks;
  size_t _num_small_chunks;
  size_t _num_medium_chunks;
  size_t _num_humongous_chunks;

  size_t _specialized_chunks_size_in_bytes;
  size_t _small_chunks_size_in_bytes;
  size_t _medium_chunks_size_in_bytes;
  size_t _humongous_chunks_size_in_bytes;

 public:
  MetaspaceChunkFreeListSummary() :
    _num_specialized_chunks(0),
    _num_small_chunks(0),
    _num_medium_chunks(0),
    _num_humongous_chunks(0),
    _specialized_chunks_size_in_bytes(0),
    _small_chunks_size_in_bytes(0),
    _medium_chunks_size_in_bytes(0),
    _humongous_chunks_size_in_bytes(0)
  {}

  MetaspaceChunkFreeListSummary(size_t num_specialized_chunks,
                                size_t num_small_chunks,
                                size_t num_medium_chunks,
                                size_t num_humongous_chunks,
                                size_t specialized_chunks_size_in_bytes,
                                size_t small_chunks_size_in_bytes,
                                size_t medium_chunks_size_in_bytes,
                                size_t humongous_chunks_size_in_bytes) :
    _num_specialized_chunks(num_specialized_chunks),
    _num_small_chunks(num_small_chunks),
    _num_medium_chunks(num_medium_chunks),
    _num_humongous_chunks(num_humongous_chunks),
    _specialized_chunks_size_in_bytes(specialized_chunks_size_in_bytes),
    _small_chunks_size_in_bytes(small_chunks_size_in_bytes),
    _medium_chunks_size_in_bytes(medium_chunks_size_in_bytes),
    _humongous_chunks_size_in_bytes(humongous_chunks_size_in_bytes)
  {}

  size_t num_specialized_chunks() const {
    return _num_specialized_chunks;
  }

  size_t num_small_chunks() const {
    return _num_small_chunks;
  }

  size_t num_medium_chunks() const {
    return _num_medium_chunks;
  }

  size_t num_humongous_chunks() const {
    return _num_humongous_chunks;
  }

  size_t specialized_chunks_size_in_bytes() const {
    return _specialized_chunks_size_in_bytes;
  }

  size_t small_chunks_size_in_bytes() const {
    return _small_chunks_size_in_bytes;
  }

  size_t medium_chunks_size_in_bytes() const {
    return _medium_chunks_size_in_bytes;
  }

  size_t humongous_chunks_size_in_bytes() const {
    return _humongous_chunks_size_in_bytes;
  }
};

#endif
