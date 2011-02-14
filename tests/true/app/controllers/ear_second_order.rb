class RolesController < ApplicationController

  def new
    # Prefills the form with 'Non member' role permissions
    @role = Role.new(params[:role] || {:permissions => Role.non_member.permissions})
    if request.post? && @role.save
      # workflow copy
      if !params[:copy_workflow_from].blank? && (copy_from = Role.find_by_id(params[:copy_workflow_from]))
        @role.workflows.copy(copy_from)
      end
      flash[:notice] = l(:notice_successful_create)
      redirect_back_or_default("something")
    end
    @permissions = @role.setable_permissions
    @roles = Role.find :all, :order => 'builtin, position'
  end

  def edit
    @role = Role.find(params[:id])
    if request.post? and @role.update_attributes(params[:role])
      flash[:notice] = l(:notice_successful_update)
      second_order()
    end
    @permissions = @role.setable_permissions
  end

  def report    
    @roles = Role.find(:all, :order => 'builtin, position')
    @permissions = Redmine::AccessControl.permissions.select { |p| !p.public? }
    if request.post?
      @roles.each do |role|
        role.permissions = params[:permissions][role.id.to_s]
        role.save
      end
      flash[:notice] = l(:notice_successful_update)
      redirect_to :action => 'index'
    end
  end
end
