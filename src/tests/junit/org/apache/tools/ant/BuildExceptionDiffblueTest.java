package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.junit.Test;

public class BuildExceptionDiffblueTest {
  /**
   * Test {@link BuildException#of(Throwable)}.
   * <ul>
   *   <li>When {@link BuildException#BuildException()}.</li>
   *   <li>Then return LocalizedMessage is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BuildException#of(Throwable)}
   */
  @Test
  public void testOf_whenBuildException_thenReturnLocalizedMessageIsNull() {
    // Arrange and Act
    BuildException actualOfResult = BuildException.of(new BuildException());

    // Assert
    assertNull(actualOfResult.getLocalizedMessage());
    assertNull(actualOfResult.getMessage());
    assertNull(actualOfResult.getCause());
    assertNull(actualOfResult.getException());
  }

  /**
   * Test {@link BuildException#of(Throwable)}.
   * <ul>
   *   <li>When {@link Throwable#Throwable()}.</li>
   *   <li>Then return LocalizedMessage is {@code Throwable}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BuildException#of(Throwable)}
   */
  @Test
  public void testOf_whenThrowable_thenReturnLocalizedMessageIsJavaLangThrowable() {
    // Arrange
    Throwable t = new Throwable();

    // Act
    BuildException actualOfResult = BuildException.of(t);

    // Assert
    assertEquals("java.lang.Throwable", actualOfResult.getLocalizedMessage());
    assertEquals("java.lang.Throwable", actualOfResult.getMessage());
    assertSame(t, actualOfResult.getCause());
    assertSame(t, actualOfResult.getException());
  }

  /**
   * Test {@link BuildException#BuildException()}.
   * <p>
   * Method under test: {@link BuildException#BuildException()}
   */
  @Test
  public void testNewBuildException() {
    // Arrange and Act
    BuildException actualBuildException = new BuildException();

    // Assert
    assertNull(actualBuildException.getLocalizedMessage());
    assertNull(actualBuildException.getMessage());
    Location location = actualBuildException.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBuildException.getCause());
    assertNull(actualBuildException.getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBuildException.getSuppressed().length);
  }

  /**
   * Test {@link BuildException#BuildException(String)}.
   * <p>
   * Method under test: {@link BuildException#BuildException(String)}
   */
  @Test
  public void testNewBuildException2() {
    // Arrange and Act
    BuildException actualBuildException = new BuildException("An error occurred");

    // Assert
    assertEquals("An error occurred", actualBuildException.getLocalizedMessage());
    assertEquals("An error occurred", actualBuildException.getMessage());
    Location location = actualBuildException.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBuildException.getCause());
    assertNull(actualBuildException.getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBuildException.getSuppressed().length);
  }

  /**
   * Test {@link BuildException#BuildException(String, Throwable)}.
   * <p>
   * Method under test: {@link BuildException#BuildException(String, Throwable)}
   */
  @Test
  public void testNewBuildException3() {
    // Arrange
    Throwable cause = new Throwable();

    // Act
    BuildException actualBuildException = new BuildException("An error occurred", cause);

    // Assert
    assertEquals("An error occurred", actualBuildException.getLocalizedMessage());
    assertEquals("An error occurred", actualBuildException.getMessage());
    Location location = actualBuildException.getLocation();
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBuildException.getSuppressed().length);
    assertSame(cause, actualBuildException.getCause());
    assertSame(cause, actualBuildException.getException());
  }

  /**
   * Test {@link BuildException#BuildException(String, Throwable, Location)}.
   * <p>
   * Method under test: {@link BuildException#BuildException(String, Throwable, Location)}
   */
  @Test
  public void testNewBuildException4() {
    // Arrange
    Throwable cause = new Throwable();
    Location location = Location.UNKNOWN_LOCATION;

    // Act
    BuildException actualBuildException = new BuildException("An error occurred", cause, location);

    // Assert
    assertEquals("An error occurred", actualBuildException.getLocalizedMessage());
    assertEquals("An error occurred", actualBuildException.getMessage());
    assertEquals(0, actualBuildException.getSuppressed().length);
    assertSame(cause, actualBuildException.getCause());
    assertSame(cause, actualBuildException.getException());
    Location expectedLocation = location.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualBuildException.getLocation());
  }

  /**
   * Test {@link BuildException#BuildException(String, Location)}.
   * <p>
   * Method under test: {@link BuildException#BuildException(String, Location)}
   */
  @Test
  public void testNewBuildException5() {
    // Arrange
    Location location = Location.UNKNOWN_LOCATION;

    // Act
    BuildException actualBuildException = new BuildException("An error occurred", location);

    // Assert
    assertEquals("An error occurred", actualBuildException.getLocalizedMessage());
    assertEquals("An error occurred", actualBuildException.getMessage());
    assertNull(actualBuildException.getCause());
    assertNull(actualBuildException.getException());
    assertEquals(0, actualBuildException.getSuppressed().length);
    Location expectedLocation = location.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualBuildException.getLocation());
  }

  /**
   * Test {@link BuildException#BuildException(String, Object[])}.
   * <p>
   * Method under test: {@link BuildException#BuildException(String, Object[])}
   */
  @Test
  public void testNewBuildException6() {
    // Arrange and Act
    BuildException actualBuildException = new BuildException("Pattern", "Format Arguments");

    // Assert
    assertEquals("Pattern", actualBuildException.getLocalizedMessage());
    assertEquals("Pattern", actualBuildException.getMessage());
    Location location = actualBuildException.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBuildException.getCause());
    assertNull(actualBuildException.getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBuildException.getSuppressed().length);
  }

  /**
   * Test {@link BuildException#BuildException(Throwable)}.
   * <p>
   * Method under test: {@link BuildException#BuildException(Throwable)}
   */
  @Test
  public void testNewBuildException7() {
    // Arrange
    Throwable cause = new Throwable();

    // Act
    BuildException actualBuildException = new BuildException(cause);

    // Assert
    assertEquals("java.lang.Throwable", actualBuildException.getLocalizedMessage());
    assertEquals("java.lang.Throwable", actualBuildException.getMessage());
    Location location = actualBuildException.getLocation();
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBuildException.getSuppressed().length);
    assertSame(cause, actualBuildException.getCause());
    assertSame(cause, actualBuildException.getException());
  }

  /**
   * Test {@link BuildException#BuildException(Throwable, Location)}.
   * <p>
   * Method under test: {@link BuildException#BuildException(Throwable, Location)}
   */
  @Test
  public void testNewBuildException8() {
    // Arrange
    Throwable cause = new Throwable();
    Location location = Location.UNKNOWN_LOCATION;

    // Act
    BuildException actualBuildException = new BuildException(cause, location);

    // Assert
    assertEquals("java.lang.Throwable", actualBuildException.getLocalizedMessage());
    assertEquals("java.lang.Throwable", actualBuildException.getMessage());
    assertEquals(0, actualBuildException.getSuppressed().length);
    assertSame(cause, actualBuildException.getCause());
    assertSame(cause, actualBuildException.getException());
    Location expectedLocation = location.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualBuildException.getLocation());
  }

  /**
   * Test {@link BuildException#getException()}.
   * <p>
   * Method under test: {@link BuildException#getException()}
   */
  @Test
  public void testGetException() {
    // Arrange, Act and Assert
    assertNull((new BuildException()).getException());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link BuildException#setLocation(Location)}
   *   <li>{@link BuildException#toString()}
   *   <li>{@link BuildException#getLocation()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    BuildException buildException = new BuildException();

    // Act
    buildException.setLocation(Location.UNKNOWN_LOCATION);
    String actualToStringResult = buildException.toString();
    Location actualLocation = buildException.getLocation();

    // Assert
    assertEquals("null", actualToStringResult);
    assertSame(actualLocation.UNKNOWN_LOCATION, actualLocation);
  }
}
