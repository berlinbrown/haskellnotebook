from django.conf.urls.defaults import *

urlpatterns = patterns('',
    # Example:
    # (r'^ghostblog/', include('ghostblog.foo.urls')),
    (r'^ghostblog/', 'ghostblog.forum.views.index'),
    (r'^forum/$', 'ghostblog.forum.views.index'),

    # Uncomment this for admin:
#     (r'^admin/', include('django.contrib.admin.urls')),
)
