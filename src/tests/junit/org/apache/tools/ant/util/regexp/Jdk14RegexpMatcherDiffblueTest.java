package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class Jdk14RegexpMatcherDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link Jdk14RegexpMatcher}
  *   <li>{@link Jdk14RegexpMatcher#setPattern(String)}
  *   <li>{@link Jdk14RegexpMatcher#getPattern()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    Jdk14RegexpMatcher actualJdk14RegexpMatcher = new Jdk14RegexpMatcher();
    actualJdk14RegexpMatcher.setPattern("Pattern");

    // Assert
    assertEquals("Pattern", actualJdk14RegexpMatcher.getPattern());
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#getCompilerOptions(int)}
   */
  @Test
  public void testGetCompilerOptions() {
    // Arrange, Act and Assert
    assertEquals(1, (new Jdk14RegexpMatcher()).getCompilerOptions(1));
    assertEquals(3, (new Jdk14RegexpMatcher()).getCompilerOptions(RegexpMatcher.MATCH_CASE_INSENSITIVE));
    assertEquals(9, (new Jdk14RegexpMatcher()).getCompilerOptions(RegexpMatcher.MATCH_MULTILINE));
    assertEquals(33, (new Jdk14RegexpMatcher()).getCompilerOptions(RegexpMatcher.MATCH_SINGLELINE));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Argument"));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups2() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jdk14RegexpMatcher.getGroups("Argument");

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups3() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Input", 1));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups4() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jdk14RegexpMatcher.getGroups("Input", 1);

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups5() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups6() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_MULTILINE));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups7() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jdk14RegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_SINGLELINE));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#matches(String)}
   */
  @Test
  public void testMatches() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Argument"));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#matches(String)}
   */
  @Test
  public void testMatches2() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("");

    // Act and Assert
    assertTrue(jdk14RegexpMatcher.matches("Argument"));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches3() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Input", 1));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches4() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("");

    // Act and Assert
    assertTrue(jdk14RegexpMatcher.matches("Input", 1));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches5() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches6() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Input", RegexpMatcher.MATCH_MULTILINE));
  }

  /**
   * Method under test: {@link Jdk14RegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches7() throws BuildException {
    // Arrange
    Jdk14RegexpMatcher jdk14RegexpMatcher = new Jdk14RegexpMatcher();
    jdk14RegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jdk14RegexpMatcher.matches("Input", RegexpMatcher.MATCH_SINGLELINE));
  }
}

