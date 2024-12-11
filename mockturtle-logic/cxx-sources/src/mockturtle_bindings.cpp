// This FFI pattern cribbed from https://luctielen.com/posts/calling_cpp_from_haskell/, credit to Luc Tielen

#include <vector>
#include <mockturtle/mockturtle.hpp>

// Suppose we want to call into this object from Haskell:

class Example
{
public:
  // Note: Constructor and destructor are auto-generated here!
  // We will have to add functions for calling these as well.
  bool do_stuff(int arg) { return !!(arg & 0x5A5A5A5A); }
  std::vector<int> const &get_values() { return _values; }

private:
  std::vector<int> _values = {3, 4, 7, 5, 6, 2, 1, 4};
};

// We can define the following FFI layer:

extern "C"
{
  struct ffi_example;

  ffi_example *example_create();
  void example_destroy(ffi_example *object);
  bool example_do_stuff(ffi_example *object, int arg);

  struct ffi_iterator; // New struct for iterating over the collection.

  ffi_iterator *ffi_iterator_create(ffi_example *obj);
  void ffi_iterator_destroy(ffi_iterator *it);
  bool ffi_iterator_has_next(ffi_iterator *it);
  int ffi_iterator_next(ffi_iterator *it);
}

struct ffi_iterator
{
  using iterator_t = std::vector<int>::const_iterator;

  // We need to keep track of what the iterator is currently pointing to,
  // as well as the end of the collection.
  iterator_t iterator;
  const iterator_t end;

  ffi_iterator(const iterator_t &begin_, const iterator_t &end_)
      : iterator(begin_), end(end_) {}
};

ffi_example *example_create()
{
  auto object = new Example();
  return reinterpret_cast<ffi_example *>(object);
}

void example_destroy(ffi_example *object)
{
  auto example = reinterpret_cast<Example *>(object);
  delete example;
}

bool example_do_stuff(ffi_example *object, int arg)
{
  auto example = reinterpret_cast<Example *>(object);
  return example->do_stuff(arg);
}

ffi_iterator *ffi_iterator_create(ffi_example *obj)
{
  // We get the collection out of the object,
  // and get the iterators pointing to beginning and end.
  auto example = reinterpret_cast<Example *>(obj);
  auto &values = example->get_values();
  return new ffi_iterator(values.begin(), values.end());
}

void ffi_iterator_destroy(ffi_iterator *it)
{
  delete it;
}

bool ffi_iterator_has_next(ffi_iterator *it)
{
  // There is a next value if the iterator is not pointing to the end.
  return it->iterator != it->end;
}

int ffi_iterator_next(ffi_iterator *it)
{
  // Get the current element, then update iterator to next element.
  auto &value = *it->iterator;
  ++it->iterator;
  return value;
}
