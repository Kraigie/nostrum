defmodule Nostrum.UtilTest do
  use ExUnit.Case, async: true

  alias Nostrum.Error.ApiError
  alias Nostrum.Util

  describe "handle_gateway_response/1" do
    test "raises ApiError with status 401 when the token is rejected" do
      error = %ApiError{status_code: 401, response: %{code: 0, message: "401: Unauthorized"}}

      assert %ApiError{status_code: 401} =
               assert_raise(ApiError, ~r/HTTP 401/, fn ->
                 Util.handle_gateway_response({:error, error})
               end)
    end

    test "raises other API errors as ApiError" do
      error = %ApiError{status_code: 500, response: %{code: 0, message: "boom"}}

      assert %ApiError{status_code: 500} =
               assert_raise(ApiError, fn -> Util.handle_gateway_response({:error, error}) end)
    end
  end

  describe "ApiError.message/1" do
    test "does not crash when the response is not a map" do
      error = %ApiError{status_code: 502, response: "Bad Gateway"}

      assert Exception.message(error) == ~s[(HTTP 502) "Bad Gateway"]
    end
  end
end
