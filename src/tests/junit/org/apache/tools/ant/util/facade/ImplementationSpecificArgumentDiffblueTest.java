package org.apache.tools.ant.util.facade;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class ImplementationSpecificArgumentDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link ImplementationSpecificArgument}
  *   <li>{@link ImplementationSpecificArgument#setImplementation(String)}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    ImplementationSpecificArgument actualImplementationSpecificArgument = new ImplementationSpecificArgument();
    actualImplementationSpecificArgument.setImplementation("Impl");

    // Assert
    assertNull(actualImplementationSpecificArgument.getDescription());
    assertNull(actualImplementationSpecificArgument.getProject());
    assertNull(actualImplementationSpecificArgument.getParts());
    Location location = actualImplementationSpecificArgument.getLocation();
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertNull(location.getFileName());
  }

  /**
   * Method under test: default or parameterless constructor of {@link ImplementationSpecificArgument}
   */
  @Test
  public void testConstructor2() {
    // Arrange and Act
    ImplementationSpecificArgument actualImplementationSpecificArgument = new ImplementationSpecificArgument();

    // Assert
    assertNull(actualImplementationSpecificArgument.getDescription());
    assertNull(actualImplementationSpecificArgument.getProject());
    assertNull(actualImplementationSpecificArgument.getParts());
  }

  /**
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetParts() {
    // Arrange, Act and Assert
    assertNull((new ImplementationSpecificArgument()).getParts("Chosen Impl"));
  }

  /**
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetParts2() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setImplementation("Impl");

    // Act and Assert
    assertEquals(0, implementationSpecificArgument.getParts("Chosen Impl").length);
  }

  /**
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetParts3() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setValue("42");

    // Act
    String[] actualParts = implementationSpecificArgument.getParts("Chosen Impl");

    // Assert
    assertEquals(1, actualParts.length);
    assertEquals("42", actualParts[0]);
  }

  /**
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetParts4() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setImplementation("Chosen Impl");

    // Act and Assert
    assertNull(implementationSpecificArgument.getParts("Chosen Impl"));
  }

  /**
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetParts5() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setPrefix("Prefix");
    implementationSpecificArgument.setValue("42");

    // Act
    String[] actualParts = implementationSpecificArgument.getParts("Chosen Impl");

    // Assert
    assertEquals(1, actualParts.length);
    assertEquals("Prefix42", actualParts[0]);
  }

  /**
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetParts6() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setSuffix("Suffix");
    implementationSpecificArgument.setValue("42");

    // Act
    String[] actualParts = implementationSpecificArgument.getParts("Chosen Impl");

    // Assert
    assertEquals(1, actualParts.length);
    assertEquals("42Suffix", actualParts[0]);
  }

  /**
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetParts7() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setLine("");

    // Act and Assert
    assertEquals(0, implementationSpecificArgument.getParts("Chosen Impl").length);
  }
}

