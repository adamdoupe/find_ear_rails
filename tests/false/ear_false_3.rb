class Admin::GroupsController < Admin::BaseController

  # POST /groups
  # POST /groups.xml
  def create
    @group = Group.new(params[:group])

    # save avatar
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
