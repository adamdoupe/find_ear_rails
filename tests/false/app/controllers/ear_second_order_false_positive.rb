class Admin::GroupsController < Admin::BaseController

  def create

    unless always_return_false_redirect_to
      return 
    end

    avatar = Avatar.create(params[:image])
    @group.avatar = avatar

    respond_to do |format|
      if @group.save
        flash[:notice] = 'Group was successfully created.'
        format.html { redirect_to(@group) }
        format.xml  { render :xml => @group, :status => :created, :location => @group }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @group.errors, :status => :unprocessable_entity }
      end
    end
  end

end
