##################################################
### Notice Update: 8/14/2007
### -------------------------- COPYRIGHT_AND_LICENSE --
### Botlist contains an open source suite of software applications for 
### social bookmarking and collecting online news content for use on the web.  
### Multiple web front-ends exist for Django, Rails, and J2EE.  
### Users and remote agents are allowed to submit interesting articles.
###
### Copyright 2007 Berlin Brown
### Copyright 2006-2007 Newspiritcompany.com
### 
### This SOURCE FILE is licensed to NEWSPIRITCOMPANY.COM.  Unless
### otherwise stated, use or distribution of this program 
### for commercial purpose is prohibited.
### 
### See LICENSE.BOTLIST for more information.
###
### The SOFTWARE PRODUCT and CODE are protected by copyright and 
### other intellectual property laws and treaties. 
###  
### Unless required by applicable law or agreed to in writing, software
### distributed  under the  License is distributed on an "AS IS" BASIS,
### WITHOUT  WARRANTIES OR CONDITIONS  OF ANY KIND, either  express  or
### implied.
##################################################

### Created: 11/4/2006
### botverse.rb
### botverse - view link listings

include_class 'org.spirit.form.ext.BotListMapEntityLink' unless defined? BotListMapEntityLink
include_class 'org.apache.commons.logging.Log' unless defined? Log
include_class 'org.apache.commons.logging.LogFactory' unless defined? LogFactory

class BotverseController
		
  def initialize(controller)
    @controller = controller
    @daohelper = @controller.entityLinksDao
    @dao_banner = @controller.adminMainBannerDao
    @dao_settings = @controller.coreSettings
    @dao_active_feeds = @controller.activeMediaFeedsDao
  end
  
  # Generate the view
  def getModel(request)
    map = BotListMapEntityLink.new    
    return map
  end
  
  # Processed when the form is submitted, 
  # see the controller 'processFormSubmission()' method
  def onSubmit(request, response, form, errors)
    return form
  end
end

BotverseController.new($controller)

## End of Script ##

