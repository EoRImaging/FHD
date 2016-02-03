FUNCTION interpolate_kernel, kernel_arr, x_offset=x_offset, y_offset=y_offset, dx=dx, dy=dy, resolution=resolution

kernel_0_0 = *kernel_arr[x_offset, y_offset]
kernel_1_0 = *kernel_arr[(x_offset+1) mod resolution, y_offset]
kernel_0_1 = *kernel_arr[x_offset, (y_offset+1) mod resolution]
kernel_1_1 = *kernel_arr[(x_offset+1) mod resolution, (y_offset+1) mod resolution]

kernel = kernel_0_0*(1-dx)*(1-dy) + kernel_1_0*(dx)*(1-dy) + kernel_0_1*(1-dx)*dy + kernel_1_1*dx*dy
RETURN, kernel
END