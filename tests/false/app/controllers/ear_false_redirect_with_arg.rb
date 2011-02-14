class Admin::SitesController < Admin::BaseController
  def update
    flash[:notice] = 'Site Settings successfully updated.'
    redirect_to :back
  end
end
