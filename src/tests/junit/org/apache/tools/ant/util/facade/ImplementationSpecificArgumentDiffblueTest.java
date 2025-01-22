package org.apache.tools.ant.util.facade;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class ImplementationSpecificArgumentDiffblueTest {
  /**
   * Test {@link ImplementationSpecificArgument#getParts(String)} with {@code String}.
   * <p>
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetPartsWithString() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setImplementation("Chosen Impl");

    // Act and Assert
    assertNull(implementationSpecificArgument.getParts("Chosen Impl"));
  }

  /**
   * Test {@link ImplementationSpecificArgument#getParts(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link ImplementationSpecificArgument} (default constructor) Implementation is {@code Impl}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetPartsWithString_givenImplementationSpecificArgumentImplementationIsImpl() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setImplementation("Impl");

    // Act and Assert
    assertEquals(0, implementationSpecificArgument.getParts("Chosen Impl").length);
  }

  /**
   * Test {@link ImplementationSpecificArgument#getParts(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link ImplementationSpecificArgument} (default constructor) Line is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetPartsWithString_givenImplementationSpecificArgumentLineIsEmptyString() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setLine("");

    // Act and Assert
    assertEquals(0, implementationSpecificArgument.getParts("Chosen Impl").length);
  }

  /**
   * Test {@link ImplementationSpecificArgument#getParts(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link ImplementationSpecificArgument} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetPartsWithString_givenImplementationSpecificArgument_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new ImplementationSpecificArgument()).getParts("Chosen Impl"));
  }

  /**
   * Test {@link ImplementationSpecificArgument#getParts(String)} with {@code String}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetPartsWithString_thenReturnArrayOfStringWith42() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setValue("42");

    // Act and Assert
    assertArrayEquals(new String[]{"42"}, implementationSpecificArgument.getParts("Chosen Impl"));
  }

  /**
   * Test {@link ImplementationSpecificArgument#getParts(String)} with {@code String}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code 42Suffix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetPartsWithString_thenReturnArrayOfStringWith42Suffix() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setSuffix("Suffix");
    implementationSpecificArgument.setValue("42");

    // Act and Assert
    assertArrayEquals(new String[]{"42Suffix"}, implementationSpecificArgument.getParts("Chosen Impl"));
  }

  /**
   * Test {@link ImplementationSpecificArgument#getParts(String)} with {@code String}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code Prefix42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImplementationSpecificArgument#getParts(String)}
   */
  @Test
  public void testGetPartsWithString_thenReturnArrayOfStringWithPrefix42() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setPrefix("Prefix");
    implementationSpecificArgument.setValue("42");

    // Act and Assert
    assertArrayEquals(new String[]{"Prefix42"}, implementationSpecificArgument.getParts("Chosen Impl"));
  }

  /**
   * Test new {@link ImplementationSpecificArgument} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ImplementationSpecificArgument}
   */
  @Test
  public void testNewImplementationSpecificArgument() {
    // Arrange and Act
    ImplementationSpecificArgument actualImplementationSpecificArgument = new ImplementationSpecificArgument();

    // Assert
    assertNull(actualImplementationSpecificArgument.getParts());
    Location location = actualImplementationSpecificArgument.getLocation();
    assertNull(location.getFileName());
    assertNull(actualImplementationSpecificArgument.getDescription());
    assertNull(actualImplementationSpecificArgument.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
