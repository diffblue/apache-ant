package org.apache.tools.ant.taskdefs.optional.testing;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class BuildTimeoutExceptionDiffblueTest {
  /**
   * Test {@link BuildTimeoutException#BuildTimeoutException()}.
   * <p>
   * Method under test: {@link BuildTimeoutException#BuildTimeoutException()}
   */
  @Test
  public void testNewBuildTimeoutException() {
    // Arrange and Act
    BuildTimeoutException actualBuildTimeoutException = new BuildTimeoutException();

    // Assert
    assertNull(actualBuildTimeoutException.getLocalizedMessage());
    assertNull(actualBuildTimeoutException.getMessage());
    Location location = actualBuildTimeoutException.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBuildTimeoutException.getCause());
    assertNull(actualBuildTimeoutException.getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBuildTimeoutException.getSuppressed().length);
  }

  /**
   * Test {@link BuildTimeoutException#BuildTimeoutException(String)}.
   * <p>
   * Method under test: {@link BuildTimeoutException#BuildTimeoutException(String)}
   */
  @Test
  public void testNewBuildTimeoutException2() {
    // Arrange and Act
    BuildTimeoutException actualBuildTimeoutException = new BuildTimeoutException("An error occurred");

    // Assert
    assertEquals("An error occurred", actualBuildTimeoutException.getLocalizedMessage());
    assertEquals("An error occurred", actualBuildTimeoutException.getMessage());
    Location location = actualBuildTimeoutException.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBuildTimeoutException.getCause());
    assertNull(actualBuildTimeoutException.getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBuildTimeoutException.getSuppressed().length);
  }

  /**
   * Test {@link BuildTimeoutException#BuildTimeoutException(String, Throwable)}.
   * <p>
   * Method under test: {@link BuildTimeoutException#BuildTimeoutException(String, Throwable)}
   */
  @Test
  public void testNewBuildTimeoutException3() {
    // Arrange
    Throwable cause = new Throwable();

    // Act
    BuildTimeoutException actualBuildTimeoutException = new BuildTimeoutException("An error occurred", cause);

    // Assert
    assertEquals("An error occurred", actualBuildTimeoutException.getLocalizedMessage());
    assertEquals("An error occurred", actualBuildTimeoutException.getMessage());
    Location location = actualBuildTimeoutException.getLocation();
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBuildTimeoutException.getSuppressed().length);
    assertSame(cause, actualBuildTimeoutException.getCause());
    assertSame(cause, actualBuildTimeoutException.getException());
  }

  /**
   * Test {@link BuildTimeoutException#BuildTimeoutException(String, Throwable, Location)}.
   * <p>
   * Method under test: {@link BuildTimeoutException#BuildTimeoutException(String, Throwable, Location)}
   */
  @Test
  public void testNewBuildTimeoutException4() {
    // Arrange
    Throwable cause = new Throwable();
    Location location = Location.UNKNOWN_LOCATION;

    // Act
    BuildTimeoutException actualBuildTimeoutException = new BuildTimeoutException("Msg", cause, location);

    // Assert
    assertEquals("Msg", actualBuildTimeoutException.getLocalizedMessage());
    assertEquals("Msg", actualBuildTimeoutException.getMessage());
    assertEquals(0, actualBuildTimeoutException.getSuppressed().length);
    assertSame(cause, actualBuildTimeoutException.getCause());
    assertSame(cause, actualBuildTimeoutException.getException());
    Location expectedLocation = location.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualBuildTimeoutException.getLocation());
  }

  /**
   * Test {@link BuildTimeoutException#BuildTimeoutException(String, Location)}.
   * <p>
   * Method under test: {@link BuildTimeoutException#BuildTimeoutException(String, Location)}
   */
  @Test
  public void testNewBuildTimeoutException5() {
    // Arrange
    Location location = Location.UNKNOWN_LOCATION;

    // Act
    BuildTimeoutException actualBuildTimeoutException = new BuildTimeoutException("An error occurred", location);

    // Assert
    assertEquals("An error occurred", actualBuildTimeoutException.getLocalizedMessage());
    assertEquals("An error occurred", actualBuildTimeoutException.getMessage());
    assertNull(actualBuildTimeoutException.getCause());
    assertNull(actualBuildTimeoutException.getException());
    assertEquals(0, actualBuildTimeoutException.getSuppressed().length);
    Location expectedLocation = location.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualBuildTimeoutException.getLocation());
  }

  /**
   * Test {@link BuildTimeoutException#BuildTimeoutException(Throwable)}.
   * <p>
   * Method under test: {@link BuildTimeoutException#BuildTimeoutException(Throwable)}
   */
  @Test
  public void testNewBuildTimeoutException6() {
    // Arrange
    Throwable cause = new Throwable();

    // Act
    BuildTimeoutException actualBuildTimeoutException = new BuildTimeoutException(cause);

    // Assert
    assertEquals("java.lang.Throwable", actualBuildTimeoutException.getLocalizedMessage());
    assertEquals("java.lang.Throwable", actualBuildTimeoutException.getMessage());
    Location location = actualBuildTimeoutException.getLocation();
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBuildTimeoutException.getSuppressed().length);
    assertSame(cause, actualBuildTimeoutException.getCause());
    assertSame(cause, actualBuildTimeoutException.getException());
  }

  /**
   * Test {@link BuildTimeoutException#BuildTimeoutException(Throwable, Location)}.
   * <p>
   * Method under test: {@link BuildTimeoutException#BuildTimeoutException(Throwable, Location)}
   */
  @Test
  public void testNewBuildTimeoutException7() {
    // Arrange
    Throwable cause = new Throwable();
    Location location = Location.UNKNOWN_LOCATION;

    // Act
    BuildTimeoutException actualBuildTimeoutException = new BuildTimeoutException(cause, location);

    // Assert
    assertEquals("java.lang.Throwable", actualBuildTimeoutException.getLocalizedMessage());
    assertEquals("java.lang.Throwable", actualBuildTimeoutException.getMessage());
    assertEquals(0, actualBuildTimeoutException.getSuppressed().length);
    assertSame(cause, actualBuildTimeoutException.getCause());
    assertSame(cause, actualBuildTimeoutException.getException());
    Location expectedLocation = location.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualBuildTimeoutException.getLocation());
  }
}
