class SeedState(object):
    def __init__(self, seed = 42):
        self.data = seed

def rand_r(rng_state):
    num = rng_state.data
    num ^= (num << 13) % (2 ** 32)
    num ^= (num >> 17) % (2 ** 32)
    num ^= (num << 5) % (2 ** 32)
    rng_state.data = num
    return num


__global_rng_state = SeedState(42)

def rand():
    global __global_rng_state
    return rand_r(__global_rng_state)

def seed(seed):
    global __global_rng_state
    __global_rng_state = SeedState(seed)

if __name__ == '__main__':
    for x in range(0, 10):
        print(rand())
