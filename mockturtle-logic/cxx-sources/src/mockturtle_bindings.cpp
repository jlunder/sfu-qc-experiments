#include <mockturtle/mockturtle.hpp>
#include <mockturtle/networks/xag.hpp>

#include <assert.h>
#include <cstdint>
#include <unordered_map>

constexpr int32_t xag_wrap_t_magic = 0x9b131af7;
constexpr int32_t xag_builder_wrap_t_magic = 0x9ec91078;
constexpr int32_t xag_reader_wrap_t_magic = 0x62aa679e;

struct xag_wrap_t {
  mockturtle::xag_network xag;
  int32_t magic;
};

struct xag_builder_wrap_t {
  mockturtle::xag_network xag;
  std::unordered_map<int32_t, mockturtle::xag_network::signal> id_map;
  int32_t magic;
};

struct xag_reader_wrap_t {
  mockturtle::xag_network xag;
  int32_t magic;
};

extern "C" {
xag_wrap_t *xag_alloc();
void xag_free(xag_wrap_t *xag_p);
xag_builder_wrap_t *xag_builder_alloc(xag_wrap_t *xag_p);
void xag_builder_free(xag_builder_wrap_t *builder_p);
void xag_builder_create_pi(xag_builder_wrap_t *builder_p, int32_t node_id);
void xag_builder_create_const(xag_builder_wrap_t *builder_p, int32_t node_id,
                              bool value);
void xag_builder_create_not(xag_builder_wrap_t *builder_p, int32_t node_id,
                            int32_t x_id);
void xag_builder_create_xor(xag_builder_wrap_t *builder_p, int32_t node_id,
                            int32_t x_id, int32_t y_id);
void xag_builder_create_and(xag_builder_wrap_t *builder_p, int32_t node_id,
                            int32_t x_id, int32_t y_id);
void xag_builder_create_po(xag_builder_wrap_t *builder_p, int32_t node_id);
xag_reader_wrap_t *xag_reader_alloc(xag_wrap_t *xag_p);
void xag_reader_free(xag_reader_wrap_t *reader_p);
}

xag_wrap_t *xag_alloc() { return new xag_wrap_t{.magic = xag_wrap_t_magic}; }

void xag_free(xag_wrap_t *xag_p) {
  assert(xag_p);
  assert(xag_p->magic == xag_wrap_t_magic);

  xag_p->magic = 0;
  delete xag_p;
}

xag_builder_wrap_t *xag_builder_alloc(xag_wrap_t *xag_p) {
  assert(xag_p);
  assert(xag_p->magic == xag_wrap_t_magic);

  return new xag_builder_wrap_t{.xag = xag_p->xag,
                                .magic = xag_builder_wrap_t_magic};
}

void xag_builder_free(xag_builder_wrap_t *builder_p) {
  assert(builder_p);
  assert(builder_p->magic == xag_builder_wrap_t_magic);

  builder_p->magic = 0;
  delete builder_p;
}

void xag_builder_create_pi(xag_builder_wrap_t *builder_p, int32_t node_id) {
  assert(builder_p);
  assert(builder_p->magic == xag_builder_wrap_t_magic);
  assert(builder_p->id_map.find(node_id) == builder_p->id_map.end());

  builder_p->id_map.emplace(node_id, builder_p->xag.create_pi());
}

void xag_builder_create_const(xag_builder_wrap_t *builder_p, int32_t node_id,
                              bool value) {
  assert(builder_p);
  assert(builder_p->magic == xag_builder_wrap_t_magic);
  assert(builder_p->id_map.find(node_id) == builder_p->id_map.end());

  builder_p->id_map.emplace(node_id, builder_p->xag.get_constant(value));
}

void xag_builder_create_not(xag_builder_wrap_t *builder_p, int32_t node_id,
                            int32_t x_id) {
  assert(builder_p);
  assert(builder_p->magic == xag_builder_wrap_t_magic);
  assert(builder_p->id_map.find(node_id) == builder_p->id_map.end());
  assert(builder_p->id_map.find(x_id) != builder_p->id_map.end());

  auto x_sig = builder_p->id_map.at(x_id);
  builder_p->id_map.emplace(node_id, builder_p->xag.create_not(x_sig));
}

void xag_builder_create_xor(xag_builder_wrap_t *builder_p, int32_t node_id,
                            int32_t x_id, int32_t y_id) {
  assert(builder_p);
  assert(builder_p->magic == xag_builder_wrap_t_magic);
  assert(builder_p->id_map.find(node_id) == builder_p->id_map.end());
  assert(builder_p->id_map.find(x_id) != builder_p->id_map.end());
  assert(builder_p->id_map.find(y_id) != builder_p->id_map.end());

  auto x_sig = builder_p->id_map.at(x_id);
  auto y_sig = builder_p->id_map.at(y_id);
  builder_p->id_map.emplace(node_id, builder_p->xag.create_xor(x_sig, y_sig));
}

void xag_builder_create_and(xag_builder_wrap_t *builder_p, int32_t node_id,
                            int32_t x_id, int32_t y_id) {
  assert(builder_p);
  assert(builder_p->magic == xag_builder_wrap_t_magic);
  assert(builder_p->id_map.find(node_id) == builder_p->id_map.end());
  assert(builder_p->id_map.find(x_id) != builder_p->id_map.end());
  assert(builder_p->id_map.find(y_id) != builder_p->id_map.end());

  auto x_sig = builder_p->id_map.at(x_id);
  auto y_sig = builder_p->id_map.at(y_id);
  builder_p->id_map.emplace(node_id, builder_p->xag.create_and(x_sig, y_sig));
}

void xag_builder_create_po(xag_builder_wrap_t *builder_p, int32_t x_id) {
  assert(builder_p);
  assert(builder_p->magic == xag_builder_wrap_t_magic);
  assert(builder_p->id_map.find(x_id) != builder_p->id_map.end());

  auto x_sig = builder_p->id_map.at(x_id);
  builder_p->xag.create_po(x_sig);
}

xag_reader_wrap_t *xag_reader_alloc(xag_wrap_t *xag_p) { return nullptr; }

void xag_reader_free(xag_reader_wrap_t *reader_p) {}

// #include <lorina/aiger.hpp>

// int main() {
//   using namespace mockturtle;

//   experiment<std::string, uint32_t, uint32_t, float, bool> exp(
//       "xag_resubstitution", "benchmark", "size_before", "size_after",
//       "runtime", "equivalent");

//   for (auto const &benchmark : epfl_benchmarks()) {
//     fmt::print("[i] processing {}\n", benchmark);

//     xag_network xag;
//     if (lorina::read_aiger(benchmark_path(benchmark), aiger_reader(xag)) !=
//         lorina::return_code::success) {
//       continue;
//     }

//     resubstitution_params ps;
//     resubstitution_stats st;
//     ps.max_pis = 8u;
//     ps.max_inserts = 1u;
//     ps.progress = false;

//     depth_view depth_xag{xag};
//     fanout_view fanout_xag{depth_xag};

//     uint32_t const size_before = fanout_xag.num_gates();
//     xag_resubstitution(fanout_xag, ps, &st);
//     xag = cleanup_dangling(xag);

//     bool const cec = benchmark == "hyp" ? true : abc_cec(fanout_xag,
//     benchmark); exp(benchmark, size_before, xag.num_gates(),
//     to_seconds(st.time_total),
//         cec);
//   }

//   exp.save();
//   exp.table();

//   return 0;
// }