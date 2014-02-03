from pylab import *
from mpl_toolkits.mplot3d import Axes3D

my_data = matplotlib.mlab.csv2rec("weekend_test_aggregated.csv")
my_data_filtered = my_data[my_data['optionwebservers']==4]
my_data_filtered_cut = my_data_filtered[['optionwordpressrequire','optionmysqlrequire','meanusertime']]
my_data_filtered_cut.sort(order=['optionmysqlrequire','optionwordpressrequire'])

X = numpy.unique(my_data_filtered_cut['optionwordpressrequire'])
Y = numpy.unique(my_data_filtered_cut['optionmysqlrequire'])
X, Y = numpy.meshgrid(X, Y)

zs = numpy.ravel(my_data_filtered_cut['meanusertime'])
Z = zs.reshape(X.shape)

fig=figure()
ax = Axes3D(fig)
ax.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap='hot')
show()
