package org.apache.tools.ant.util.facade;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class FacadeTaskHelperDiffblueTest {
  /**
   * Method under test: {@link FacadeTaskHelper#addImplementationArgument(ImplementationSpecificArgument)}
   */
  @Test
  public void testAddImplementationArgument() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");

    // Act
    facadeTaskHelper.addImplementationArgument(new ImplementationSpecificArgument());

    // Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>{@link FacadeTaskHelper#FacadeTaskHelper(String)}
  *   <li>{@link FacadeTaskHelper#setImplementation(String)}
  *   <li>{@link FacadeTaskHelper#setMagicValue(String)}
  *   <li>{@link FacadeTaskHelper#getExplicitChoice()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    FacadeTaskHelper actualFacadeTaskHelper = new FacadeTaskHelper("42");
    actualFacadeTaskHelper.setImplementation("User Choice");
    actualFacadeTaskHelper.setMagicValue("42");

    // Assert
    assertEquals("User Choice", actualFacadeTaskHelper.getExplicitChoice());
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link FacadeTaskHelper#FacadeTaskHelper(String, String)}
   *   <li>{@link FacadeTaskHelper#setImplementation(String)}
   *   <li>{@link FacadeTaskHelper#setMagicValue(String)}
   *   <li>{@link FacadeTaskHelper#getExplicitChoice()}
   * </ul>
   */
  @Test
  public void testConstructor2() {
    // Arrange and Act
    FacadeTaskHelper actualFacadeTaskHelper = new FacadeTaskHelper("42", "42");
    actualFacadeTaskHelper.setImplementation("User Choice");
    actualFacadeTaskHelper.setMagicValue("42");

    // Assert
    assertEquals("User Choice", actualFacadeTaskHelper.getExplicitChoice());
  }

  /**
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs() {
    // Arrange, Act and Assert
    assertEquals(0, (new FacadeTaskHelper("42")).getArgs().length);
    assertEquals(0, (new FacadeTaskHelper("42", "42")).getArgs().length);
  }

  /**
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs2() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.addImplementationArgument(new ImplementationSpecificArgument());

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs3() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.addImplementationArgument(new ImplementationSpecificArgument());
    facadeTaskHelper.addImplementationArgument(new ImplementationSpecificArgument());

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs4() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.setImplementation("User Choice");
    facadeTaskHelper.addImplementationArgument(new ImplementationSpecificArgument());

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
   * Method under test: {@link FacadeTaskHelper#getArgs()}
   */
  @Test
  public void testGetArgs5() {
    // Arrange
    ImplementationSpecificArgument implementationSpecificArgument = new ImplementationSpecificArgument();
    implementationSpecificArgument.setImplementation("Impl");

    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.addImplementationArgument(implementationSpecificArgument);

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getArgs().length);
  }

  /**
   * Method under test: {@link FacadeTaskHelper#getImplementation()}
   */
  @Test
  public void testGetImplementation() {
    // Arrange, Act and Assert
    assertEquals("42", (new FacadeTaskHelper("42")).getImplementation());
    assertEquals("42", (new FacadeTaskHelper("42", "42")).getImplementation());
  }

  /**
   * Method under test: {@link FacadeTaskHelper#getImplementation()}
   */
  @Test
  public void testGetImplementation2() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.setImplementation("User Choice");

    // Act and Assert
    assertEquals("User Choice", facadeTaskHelper.getImplementation());
  }

  /**
   * Method under test: {@link FacadeTaskHelper#getImplementationClasspath(Project)}
   */
  @Test
  public void testGetImplementationClasspath() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");

    // Act and Assert
    assertEquals(0, facadeTaskHelper.getImplementationClasspath(new Project()).size());
  }

  /**
   * Method under test: {@link FacadeTaskHelper#hasBeenSet()}
   */
  @Test
  public void testHasBeenSet() {
    // Arrange, Act and Assert
    assertFalse((new FacadeTaskHelper("42")).hasBeenSet());
    assertTrue((new FacadeTaskHelper("42", "42")).hasBeenSet());
  }

  /**
   * Method under test: {@link FacadeTaskHelper#hasBeenSet()}
   */
  @Test
  public void testHasBeenSet2() {
    // Arrange
    FacadeTaskHelper facadeTaskHelper = new FacadeTaskHelper("42");
    facadeTaskHelper.setImplementation("User Choice");

    // Act and Assert
    assertTrue(facadeTaskHelper.hasBeenSet());
  }
}

