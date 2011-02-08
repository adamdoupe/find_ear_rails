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

