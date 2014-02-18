from pylab import *
from mpl_toolkits.mplot3d import Axes3D

optionwebservers = 5

my_data = matplotlib.mlab.csv2rec("tuesday_test.csv")
my_data_filtered = my_data[my_data['optionwebservers'] == optionwebservers]
my_data_filtered_cut = my_data_filtered[['optionwordpressrequire','optionmysqlrequire','usertime']]
my_data_filtered_cut.sort(order=['optionmysqlrequire','optionwordpressrequire'])

X = numpy.unique(my_data_filtered_cut['optionwordpressrequire'])
Y = numpy.unique(my_data_filtered_cut['optionmysqlrequire'])
X, Y = numpy.meshgrid(X, Y)

zs = numpy.ravel(my_data_filtered_cut['usertime'])
Z = zs.reshape(X.shape)

fig=figure()
ax = Axes3D(fig)
ax.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap='coolwarm')
show()
