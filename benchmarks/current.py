from pylab import *
from mpl_toolkits.mplot3d import Axes3D

my_data = matplotlib.mlab.csv2rec("monday_evening_aggregated.csv.gz")

subplot_params = matplotlib.figure.SubplotParams(left=0.01, bottom=0.01, right=0.99, top=0.97, wspace=0.03, hspace=0.03)

solver_indices = {
	"g12"    : 0,
	"gecode" : 1
}

webserver_indices = {
	0 : 0,
	1 : 1,
	3 : 2,
	4 : 3
}

value_field_in_table = {
	"total time"      : 'meanusertime',
	"solving time"    : 'meansolvingusertime'
}

axis_fontsize = 26
tick_fontsize = 22

for value_field in ["total time"]:#, "solving time"]: #, "generating time"]:
	for optionmysqlprovide in [3]:
		#fig=figure(tight_layout=True)
		for solver in ["gecode", "g12"]:
			for optionwebservers in [0,1,3,4]:
				fig=figure(figsize=(15,8))#, dpi=150)#, subplotpars=subplot_params)
				
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
				
				#ax = fig.add_subplot(2, 2, 1 + webserver_indices[optionwebservers] + (solver_indices[solver] * 2), projection='3d')
				ax = Axes3D(fig)
				#ax = fig.add_subplot(1, 1, 1, projection='3d')
				#ax.set_title("Solver: %s\nMySQL provide: %d\nWebservers: %d" % (solver, optionmysqlprovide, optionwebservers))
				#ax.set_title("Webservers: %d" % (optionwebservers))
				ax.set_xlabel(r'wordpress backend $\geqslant$ n', fontsize = axis_fontsize)
				ax.set_ylabel(r'mysql $\geqslant$ n'            , fontsize = axis_fontsize)
				ax.set_zlabel(value_field + " (s)"              , fontsize = axis_fontsize)
				ax.tick_params(labelsize = tick_fontsize)
				ax.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap='gist_yarg')
				show()

				for extension in ["eps", "pdf"]:
					fig.savefig('chart_%s_webservers_%d.%s' % (solver, optionwebservers, extension), bbox_inches='tight', dpi=100)
				