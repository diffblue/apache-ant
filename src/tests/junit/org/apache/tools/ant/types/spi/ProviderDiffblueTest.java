package org.apache.tools.ant.types.spi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class ProviderDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Provider#setClassName(String)}
   *   <li>{@link Provider#getClassName()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Provider provider = new Provider();

    // Act
    provider.setClassName("Type");

    // Assert
    assertEquals("Type", provider.getClassName());
  }

  /**
   * Test {@link Provider#check()}.
   * <ul>
   *   <li>Given {@link Provider} (default constructor) ClassName is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Provider#check()}
   */
  @Test
  public void testCheck_givenProviderClassNameIsEmptyString_thenThrowBuildException() {
    // Arrange
    Provider provider = new Provider();
    provider.setClassName("");

    // Act and Assert
    assertThrows(BuildException.class, () -> provider.check());
  }

  /**
   * Test {@link Provider#check()}.
   * <ul>
   *   <li>Given {@link Provider} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Provider#check()}
   */
  @Test
  public void testCheck_givenProvider_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Provider()).check());
  }

  /**
   * Test new {@link Provider} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Provider}
   */
  @Test
  public void testNewProvider() {
    // Arrange and Act
    Provider actualProvider = new Provider();

    // Assert
    Location location = actualProvider.getLocation();
    assertNull(location.getFileName());
    assertNull(actualProvider.getDescription());
    assertNull(actualProvider.getClassName());
    assertNull(actualProvider.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
