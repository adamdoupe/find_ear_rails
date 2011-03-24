class UsersController < ApplicationController

  def test
    this_shouldnt_be_a_problem
    if true
      redirect_to
    end

    Users.create()

  end
end
