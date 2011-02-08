class TopicsController < ApplicationController
  before_filter :fetch_topic, :only => [:show]
  before_filter :login_required, :only => [:show], :if => :must_login 
  before_filter :login_required, :except => [:index, :show, :search]
  cache_sweeper :topics_sweeper, :only => [ :create, :update, :destroy, :toggle_status ]
  
  # GETs should be safe (see http://www.w3.org/2001/tag/doc/whenToUseGet.html)
  verify :method => :post, :only => [:create, :rate ],
  :redirect_to => { :action => :index }
  verify :method => :put, :only => [ :update, :toggle_status ],
  :redirect_to => { :action => :index }
  verify :method => :delete, :only => [ :destroy ],
  :redirect_to => { :action => :index }
  
  def update
    @topic = Topic.find(params[:id])
    unless params[:topic][:owner_id].nil? or params[:topic][:owner_id].empty?
      # was owner updated? If so, make sure they are watching this topic
      @user = User.find params[:topic][:owner_id]
      # Add watcher unless owner is already watching or the owner did not change
      @topic.watchers << @user unless @topic.watchers.include? @user or @topic.owner == @user
    end
    
    unless @topic.forum.can_edit? current_user or prodmgr?
      logger.debug "Can't edit, oh noes!"
      flash[:error] = ForumsController.flash_for_forum_access_denied(current_user)
      redirect_to redirect_path_on_access_denied(current_user)
    end
    logger.debug params[:topic]
    if @topic.update_attributes(params[:topic])
      flash[:notice] = "Topic '#{@topic.title}' was successfully updated."
      redirect_to topic_path(@topic)
    else
      render :action => :edit
    end
  end
  
end
