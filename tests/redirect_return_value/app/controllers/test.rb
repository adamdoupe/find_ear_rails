class BugreportController < ApplicationController

  def always_true_1
    something = method_call()
    if something
      redirect_to 'testing'
      return true
    end
  end

  def always_nil_1
    if method_call()
      redirect_to 'something'
      return nil
    end
  end

  def always_false
    if method_call()
      redirect_to 'something'
      return false
    end
  end

  def can_return_anything
    if method_call()
      redirect_to 'something'
      return false
    else
      redirect_to 'another_thing'
      return true
    end
  end

  def multiple_branches_true
    something()
    if method_call()
      redirect_to 'something'
      return true
    else
      redirect_to 'another_thing'
      return true
    end
  end

  def multiple_branches_false
    something()
    if method_call()
      redirect_to 'something'
      return false
    else
      redirect_to 'another_thing'
      return false
    end
  end

  def far_away_return_true
    something()
    if method_call("adam")
      redirect_to 'something'
    end

    return true
  end

  def far_away_return_false
    something()
    if method_call("adam")
      redirect_to 'something'
    end
    something_else()
    return false
  end

  def always_return_false_redirect_to
    if something() 
      redirect_to '/home'
      return false
    end
    return true
  end

  def never_call_redirect
    if something()
      return "blah"
    end
    return true
  end

  def no_explicit_return_always_true
    redirect_to url_for_group(@group)
  end
end

