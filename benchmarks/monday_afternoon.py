from pylab import *
from mpl_toolkits.mplot3d import Axes3D

my_data = matplotlib.mlab.csv2rec("monday_afternoon_aggregated.csv.gz")

subplot_params = matplotlib.figure.SubplotParams(left=0.01, bottom=0.01, right=0.99, top=0.97, wspace=0.03, hspace=0.03)

solver_indices = {
	"g12"    : 0,
	"gecode" : 1
}

webserver_indices = {
	0 : 0,
	3 : 1
}

value_field_in_table = {
	"total time"      : 'meanusertime',
	"solving time"    : 'meansolvingusertime'
}

for value_field in ["total time", "solving time"]: #, "generating time"]:
	for optionmysqlprovide in [3]:
		fig=figure(figsize=(15,8), subplotpars=subplot_params)
		#fig=figure(tight_layout=True)
		for solver in ["g12", "gecode"]:
			for optionwebservers in [0,3]:
				
				my_data_filtered = my_data[ (my_data['solver'] == solver) & (my_data['optionmysqlprovide'] == optionmysqlprovide) & (my_data['optionwebservers'] == optionwebservers) ]
				my_data_filtered.sort(order=['optionmysqlrequire','optionwordpressrequire'])
				
				X = numpy.unique(my_data_filtered['optionwordpressrequire'])
				Y = numpy.unique(my_data_filtered['optionmysqlrequire'])
				X, Y = numpy.meshgrid(X, Y)
				
				if( (value_field == "total time") or (value_field == "solving time") ):
					zs = numpy.ravel(my_data_filtered[value_field_in_table[value_field]])
					Z = zs.reshape(X.shape)
				elif (value_field == "generating time"):
					zs_total   = numpy.ravel(my_data_filtered[value_field_in_table["total time"]])
					zs_solving = numpy.ravel(my_data_filtered[value_field_in_table["solving time"]])
					zs = zs_total - zs_solving
					Z = zs.reshape(X.shape)
				
				ax = fig.add_subplot(2, 2, 1 + webserver_indices[optionwebservers] + (solver_indices[solver] * 2), projection='3d')
				ax.set_title("Solver: %s\nMySQL provide: %d\nWebservers: %d" % (solver, optionmysqlprovide, optionwebservers))
				#ax.set_title("Webservers: %d" % (optionwebservers))
				ax.set_xlabel("Wordpress require")
				ax.set_ylabel("MySQL require")
				#ax.set_yticks([0,20,40,60,80,100])
				ax.set_zlabel(value_field + " (s)")
				ax.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap='coolwarm')
				show()
				