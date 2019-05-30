#ifndef CPU_X86_VM_VM_VERSION_EXT_X86_HPP
#define CPU_X86_VM_VM_VERSION_EXT_X86_HPP

#include "utilities/macros.hpp"
#include "vm_version_x86.hpp"

class VM_Version_Ext : public VM_Version {
 private:
  static const size_t      VENDOR_LENGTH;
  static const size_t      CPU_EBS_MAX_LENGTH;
  static const size_t      CPU_TYPE_DESC_BUF_SIZE;
  static const size_t      CPU_DETAILED_DESC_BUF_SIZE;

  static const char* const _family_id_intel[];
  static const char* const _family_id_amd[];
  static const char* const _brand_id[];
  static const char* const _model_id_pentium_pro[];

  static const char* const _feature_edx_id[];
  static const char* const _feature_extended_edx_id[];
  static const char* const _feature_ecx_id[];
  static const char* const _feature_extended_ecx_id[];

  static int               _no_of_threads;
  static int               _no_of_cores;
  static int               _no_of_packages;
  static char*             _cpu_brand_string;
  static jlong             _max_qualified_cpu_frequency;

  static const char* cpu_family_description(void);
  static const char* cpu_model_description(void);
  static const char* cpu_brand(void);
  static const char* cpu_brand_string(void);

  static int cpu_type_description(char* const buf, size_t buf_len);
  static int cpu_detailed_description(char* const buf, size_t buf_len);
  static int cpu_extended_brand_string(char* const buf, size_t buf_len);

  static bool cpu_is_em64t(void);
  static bool is_netburst(void);

  // Returns bytes written excluding termninating null byte.
  static size_t cpu_write_support_string(char* const buf, size_t buf_len);
  static void resolve_cpu_information_details(void);
  static jlong max_qualified_cpu_freq_from_brand_string(void);

 public:
  // Offsets for cpuid asm stub brand string
  static ByteSize proc_name_0_offset() { return byte_offset_of(CpuidInfo, proc_name_0); }
  static ByteSize proc_name_1_offset() { return byte_offset_of(CpuidInfo, proc_name_1); }
  static ByteSize proc_name_2_offset() { return byte_offset_of(CpuidInfo, proc_name_2); }
  static ByteSize proc_name_3_offset() { return byte_offset_of(CpuidInfo, proc_name_3); }
  static ByteSize proc_name_4_offset() { return byte_offset_of(CpuidInfo, proc_name_4); }
  static ByteSize proc_name_5_offset() { return byte_offset_of(CpuidInfo, proc_name_5); }
  static ByteSize proc_name_6_offset() { return byte_offset_of(CpuidInfo, proc_name_6); }
  static ByteSize proc_name_7_offset() { return byte_offset_of(CpuidInfo, proc_name_7); }
  static ByteSize proc_name_8_offset() { return byte_offset_of(CpuidInfo, proc_name_8); }
  static ByteSize proc_name_9_offset() { return byte_offset_of(CpuidInfo, proc_name_9); }
  static ByteSize proc_name_10_offset() { return byte_offset_of(CpuidInfo, proc_name_10); }
  static ByteSize proc_name_11_offset() { return byte_offset_of(CpuidInfo, proc_name_11); }

  static int number_of_threads(void);
  static int number_of_cores(void);
  static int number_of_sockets(void);

  static jlong maximum_qualified_cpu_frequency(void);

  static bool supports_tscinv_ext(void);

  static const char* cpu_name(void);
  static const char* cpu_description(void);

  static void initialize();
};

#endif
