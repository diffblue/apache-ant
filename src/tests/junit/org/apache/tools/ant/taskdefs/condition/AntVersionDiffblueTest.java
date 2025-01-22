package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class AntVersionDiffblueTest {
  /**
   * Test {@link AntVersion#execute()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) AtLeast is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#execute()}
   */
  @Test
  public void testExecute_givenAntVersionAtLeastIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setProperty("foo");
    antVersion.setAtLeast("foo");
    antVersion.setExactly(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> antVersion.execute());
  }

  /**
   * Test {@link AntVersion#execute()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) AtLeast is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#execute()}
   */
  @Test
  public void testExecute_givenAntVersionAtLeastIsFoo_thenThrowBuildException2() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setProperty("foo");
    antVersion.setAtLeast("foo");
    antVersion.setExactly("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> antVersion.execute());
  }

  /**
   * Test {@link AntVersion#execute()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) Exactly is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#execute()}
   */
  @Test
  public void testExecute_givenAntVersionExactlyIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setProperty("foo");
    antVersion.setAtLeast(null);
    antVersion.setExactly("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> antVersion.execute());
  }

  /**
   * Test {@link AntVersion#execute()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#execute()}
   */
  @Test
  public void testExecute_givenAntVersion_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new AntVersion()).execute());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) AtLeast is {@code 1.8}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersionAtLeastIs18_thenReturnTrue() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setAtLeast("1.8");
    antVersion.setExactly(null);

    // Act and Assert
    assertTrue(antVersion.eval());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) AtLeast is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersionAtLeastIs42_thenReturnFalse() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setAtLeast("42");
    antVersion.setExactly(null);

    // Act and Assert
    assertFalse(antVersion.eval());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) AtLeast is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersionAtLeastIsEmptyString_thenReturnTrue() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setAtLeast("");
    antVersion.setExactly(null);

    // Act and Assert
    assertTrue(antVersion.eval());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) AtLeast is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersionAtLeastIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setAtLeast("foo");
    antVersion.setExactly(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> antVersion.eval());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) AtLeast is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersionAtLeastIsFoo_thenThrowBuildException2() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setAtLeast("foo");
    antVersion.setExactly("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> antVersion.eval());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) Exactly is {@code 1.8}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersionExactlyIs18_thenReturnFalse() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setAtLeast(null);
    antVersion.setExactly("1.8");

    // Act and Assert
    assertFalse(antVersion.eval());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) Exactly is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersionExactlyIs42_thenReturnFalse() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setAtLeast(null);
    antVersion.setExactly("42");

    // Act and Assert
    assertFalse(antVersion.eval());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) Exactly is empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersionExactlyIsEmptyString_thenReturnFalse() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setAtLeast(null);
    antVersion.setExactly("");

    // Act and Assert
    assertFalse(antVersion.eval());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor) Exactly is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersionExactlyIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    AntVersion antVersion = new AntVersion();
    antVersion.setAtLeast(null);
    antVersion.setExactly("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> antVersion.eval());
  }

  /**
   * Test {@link AntVersion#eval()}.
   * <ul>
   *   <li>Given {@link AntVersion} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntVersion#eval()}
   */
  @Test
  public void testEval_givenAntVersion_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new AntVersion()).eval());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link AntVersion#setAtLeast(String)}
   *   <li>{@link AntVersion#setExactly(String)}
   *   <li>{@link AntVersion#setProperty(String)}
   *   <li>{@link AntVersion#getAtLeast()}
   *   <li>{@link AntVersion#getExactly()}
   *   <li>{@link AntVersion#getProperty()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    AntVersion antVersion = new AntVersion();

    // Act
    antVersion.setAtLeast("At Least");
    antVersion.setExactly("Exactly");
    antVersion.setProperty("Propertyname");
    String actualAtLeast = antVersion.getAtLeast();
    String actualExactly = antVersion.getExactly();

    // Assert
    assertEquals("At Least", actualAtLeast);
    assertEquals("Exactly", actualExactly);
    assertEquals("Propertyname", antVersion.getProperty());
  }

  /**
   * Test new {@link AntVersion} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AntVersion}
   */
  @Test
  public void testNewAntVersion() {
    // Arrange and Act
    AntVersion actualAntVersion = new AntVersion();

    // Assert
    assertNull(actualAntVersion.getDescription());
    assertNull(actualAntVersion.getTaskName());
    assertNull(actualAntVersion.getTaskType());
    assertNull(actualAntVersion.getAtLeast());
    assertNull(actualAntVersion.getExactly());
    assertNull(actualAntVersion.getProperty());
    assertNull(actualAntVersion.getProject());
    assertNull(actualAntVersion.getOwningTarget());
  }
}
