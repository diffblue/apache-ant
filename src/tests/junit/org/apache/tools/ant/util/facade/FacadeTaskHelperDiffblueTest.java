package org.apache.tools.ant.util.facade;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class FacadeTaskHelperDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link FacadeTaskHelper#FacadeTaskHelper(String)}
   *   <li>{@link FacadeTaskHelper#setImplementation(String)}
   *   <li>{@link FacadeTaskHelper#setMagicValue(String)}
   *   <li>{@link FacadeTaskHelper#getExplicitChoice()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    FacadeTaskHelper actualFacadeTaskHelper = new FacadeTaskHelper("42");
    actualFacadeTaskHelper.setImplementation("User Choice");
    actualFacadeTaskHelper.setMagicValue("42");

    // Assert
    assertEquals("User Choice", actualFacadeTaskHelper.getExplicitChoice());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link FacadeTaskHelper#FacadeTaskHelper(String, String)}
   *   <li>{@link FacadeTaskHelper#setImplementation(String)}
   *   <li>{@link FacadeTaskHelper#setMagicValue(String)}
   *   <li>{@link FacadeTaskHelper#getExplicitChoice()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters2() {
    // Arrange and Act
    FacadeTaskHelper actualFacadeTaskHelper = new FacadeTaskHelper("42", "42");
    actualFacadeTaskHelper.setImplementation("User Choice");
    actualFacadeTaskHelper.setMagicValue("42");

    // Assert
    assertEquals("User Choice", actualFacadeTaskHelper.getExplicitChoice());
  }

  /**
   * Test {@link FacadeTaskHelper#getImplementation()}.
   * <p>
   * Method under test: {@link FacadeTaskHelper#getImplementation()}
   */
  @Test
  public void testGetImplementation() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.setImplementation(null);
    facadeTaskHelper.setMagicValue("foo");

    // Act and Assert
    assertEquals("foo", facadeTaskHelper.getImplementation());
  }

  /**
   * Test {@link FacadeTaskHelper#getImplementation()}.
   * <p>
   * Method under test: {@link FacadeTaskHelper#getImplementation()}
   */
  @Test
  public void testGetImplementation2() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.setImplementation("foo");
    facadeTaskHelper.setMagicValue(null);

    // Act and Assert
    assertEquals("foo", facadeTaskHelper.getImplementation());
  }

  /**
   * Test {@link FacadeTaskHelper#getImplementation()}.
   * <ul>
   *   <li>Given {@link FacadeTaskHelper#FacadeTaskHelper(String)} with defaultValue is {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getImplementation()}
   */
  @Test
  public void testGetImplementation_givenFacadeTaskHelperWithDefaultValueIs42_thenReturn42() {
    // Arrange, Act and Assert
    assertEquals("42", (new FacadeTaskHelper("42")).getImplementation());
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs() {
    // Arrange
    ImplementationSpecificArgument arg = new ImplementationSpecificArgument();
    arg.setImplementation("Impl");

    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("Impl");
    facadeTaskHelper.addImplementationArgument(arg);

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <ul>
   *   <li>Given {@link FacadeTaskHelper#FacadeTaskHelper(String)} with defaultValue is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs_givenFacadeTaskHelperWithDefaultValueIs42() {
    // Arrange, Act and Assert
    assertEquals(0, (new FacadeTaskHelper("42")).getArgs().length);
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <ul>
   *   <li>Given {@link FacadeTaskHelper#FacadeTaskHelper(String, String)} with defaultValue is {@code 42} and magicValue is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs_givenFacadeTaskHelperWithDefaultValueIs42AndMagicValueIs42() {
    // Arrange, Act and Assert
    assertEquals(0, (new FacadeTaskHelper("42", "42")).getArgs().length);
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <ul>
   *   <li>Given {@link FacadeTaskHelper#FacadeTaskHelper(String)} with defaultValue is {@code 42} Implementation is {@code User Choice}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs_givenFacadeTaskHelperWithDefaultValueIs42ImplementationIsUserChoice() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.setImplementation("User Choice");
    facadeTaskHelper.addImplementationArgument(new ImplementationSpecificArgument());

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <ul>
   *   <li>Given {@link ImplementationSpecificArgument} (default constructor) Implementation is {@code Impl}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs_givenImplementationSpecificArgumentImplementationIsImpl() {
    // Arrange
    ImplementationSpecificArgument arg = new ImplementationSpecificArgument();
    arg.setImplementation("Impl");

    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.addImplementationArgument(arg);

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <ul>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs_thenReturnArrayLengthIsZero() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.addImplementationArgument(new ImplementationSpecificArgument());

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <ul>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs_thenReturnArrayLengthIsZero2() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.addImplementationArgument(new ImplementationSpecificArgument());
    facadeTaskHelper.addImplementationArgument(new ImplementationSpecificArgument());

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs_thenReturnArrayOfStringWith42() {
    // Arrange
    ImplementationSpecificArgument arg = new ImplementationSpecificArgument();
    arg.setValue("42");

    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.addImplementationArgument(arg);

    // Act and Assert
    assertArrayEquals(new String[]{"42"}, facadeTaskHelper.getArgs());
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code 42Suffix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs_thenReturnArrayOfStringWith42Suffix() {
    // Arrange
    ImplementationSpecificArgument arg = new ImplementationSpecificArgument();
    arg.setSuffix("Suffix");
    arg.setValue("42");

    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.addImplementationArgument(arg);

    // Act and Assert
    assertArrayEquals(new String[]{"42Suffix"}, facadeTaskHelper.getArgs());
  }

  /**
   * Test {@link FacadeTaskHelper#getArgs()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code Prefix42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs_thenReturnArrayOfStringWithPrefix42() {
    // Arrange
    ImplementationSpecificArgument arg = new ImplementationSpecificArgument();
    arg.setPrefix("Prefix");
    arg.setValue("42");

    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.addImplementationArgument(arg);

    // Act and Assert
    assertArrayEquals(new String[]{"Prefix42"}, facadeTaskHelper.getArgs());
  }

  /**
   * Test {@link FacadeTaskHelper#hasBeenSet()}.
   * <ul>
   *   <li>Given {@link FacadeTaskHelper#FacadeTaskHelper(String)} with defaultValue is {@code 42} Implementation is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#hasBeenSet()}
   */
  @Test
  public void testHasBeenSet_givenFacadeTaskHelperWithDefaultValueIs42ImplementationIsFoo() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.setImplementation("foo");
    facadeTaskHelper.setMagicValue(null);

    // Act and Assert
    assertTrue(facadeTaskHelper.hasBeenSet());
  }

  /**
   * Test {@link FacadeTaskHelper#hasBeenSet()}.
   * <ul>
   *   <li>Given {@link FacadeTaskHelper#FacadeTaskHelper(String)} with defaultValue is {@code 42} Implementation is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#hasBeenSet()}
   */
  @Test
  public void testHasBeenSet_givenFacadeTaskHelperWithDefaultValueIs42ImplementationIsNull() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.setImplementation(null);
    facadeTaskHelper.setMagicValue("foo");

    // Act and Assert
    assertTrue(facadeTaskHelper.hasBeenSet());
  }

  /**
   * Test {@link FacadeTaskHelper#hasBeenSet()}.
   * <ul>
   *   <li>Given {@link FacadeTaskHelper#FacadeTaskHelper(String)} with defaultValue is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FacadeTaskHelper#hasBeenSet()}
   */
  @Test
  public void testHasBeenSet_givenFacadeTaskHelperWithDefaultValueIs42_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new FacadeTaskHelper("42")).hasBeenSet());
  }

  /**
   * Test {@link FacadeTaskHelper#getImplementationClasspath(Project)}.
   * <p>
   * Method under test: {@link FacadeTaskHelper#getImplementationClasspath(Project)}
   */
  @Test
  public void testGetImplementationClasspath() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    Project project = new Project();

    // Act
    Path actualImplementationClasspath = facadeTaskHelper.getImplementationClasspath(project);

    // Assert
    assertNull(actualImplementationClasspath.getDescription());
    assertNull(actualImplementationClasspath.getRefid());
    assertEquals(0, actualImplementationClasspath.size());
    assertFalse(actualImplementationClasspath.isReference());
    assertTrue(actualImplementationClasspath.isEmpty());
    assertSame(project, actualImplementationClasspath.getProject());
  }
}
