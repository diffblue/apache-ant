package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Vector;
import org.apache.regexp.RE;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class JakartaRegexpMatcherDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link JakartaRegexpMatcher}
  *   <li>{@link JakartaRegexpMatcher#setPattern(String)}
  *   <li>{@link JakartaRegexpMatcher#getPattern()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    JakartaRegexpMatcher actualJakartaRegexpMatcher = new JakartaRegexpMatcher();
    actualJakartaRegexpMatcher.setPattern("Pattern");

    // Assert
    assertEquals("Pattern", actualJakartaRegexpMatcher.getPattern());
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act
    RE actualCompiledPattern = jakartaRegexpMatcher.getCompiledPattern(1);

    // Assert
    assertEquals(0, actualCompiledPattern.getMatchFlags());
    assertEquals(Regexp.REPLACE_ALL, actualCompiledPattern.getProgram().getInstructions().length);
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern2() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("");

    // Act
    RE actualCompiledPattern = jakartaRegexpMatcher.getCompiledPattern(1);

    // Assert
    assertEquals(0, actualCompiledPattern.getMatchFlags());
    assertEquals(9, actualCompiledPattern.getProgram().getInstructions().length);
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern3() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act
    RE actualCompiledPattern = jakartaRegexpMatcher.getCompiledPattern(RegexpMatcher.MATCH_CASE_INSENSITIVE);

    // Assert
    assertEquals(1, actualCompiledPattern.getMatchFlags());
    assertEquals(Regexp.REPLACE_ALL, actualCompiledPattern.getProgram().getInstructions().length);
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern4() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act
    RE actualCompiledPattern = jakartaRegexpMatcher.getCompiledPattern(RegexpMatcher.MATCH_MULTILINE);

    // Assert
    assertEquals(2, actualCompiledPattern.getMatchFlags());
    assertEquals(Regexp.REPLACE_ALL, actualCompiledPattern.getProgram().getInstructions().length);
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern5() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act
    RE actualCompiledPattern = jakartaRegexpMatcher.getCompiledPattern(RegexpMatcher.MATCH_SINGLELINE);

    // Assert
    assertEquals(4, actualCompiledPattern.getMatchFlags());
    assertEquals(Regexp.REPLACE_ALL, actualCompiledPattern.getProgram().getInstructions().length);
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getCompilerOptions(int)}
   */
  @Test
  public void testGetCompilerOptions() {
    // Arrange, Act and Assert
    assertEquals(0, (new JakartaRegexpMatcher()).getCompilerOptions(1));
    assertEquals(1, (new JakartaRegexpMatcher()).getCompilerOptions(RegexpMatcher.MATCH_CASE_INSENSITIVE));
    assertEquals(2, (new JakartaRegexpMatcher()).getCompilerOptions(RegexpMatcher.MATCH_MULTILINE));
    assertEquals(4, (new JakartaRegexpMatcher()).getCompilerOptions(RegexpMatcher.MATCH_SINGLELINE));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaRegexpMatcher.getGroups("Argument"));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups2() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jakartaRegexpMatcher.getGroups("Argument");

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups3() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act
    Vector<String> actualGroups = jakartaRegexpMatcher.getGroups("42");

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("42", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups4() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaRegexpMatcher.getGroups("Input", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups5() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jakartaRegexpMatcher.getGroups("Input", 1);

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups6() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act and Assert
    assertNull(jakartaRegexpMatcher.getGroups("Input", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups7() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaRegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups8() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaRegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_MULTILINE));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups9() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaRegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_SINGLELINE));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups10() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act
    Vector<String> actualGroups = jakartaRegexpMatcher.getGroups("42", 1);

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("42", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups11() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act and Assert
    assertNull(jakartaRegexpMatcher.getGroups("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups12() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act
    Vector<String> actualGroups = jakartaRegexpMatcher.getGroups("42", RegexpMatcher.MATCH_CASE_INSENSITIVE);

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("42", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String)}
   */
  @Test
  public void testMatches() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaRegexpMatcher.matches("Argument"));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String)}
   */
  @Test
  public void testMatches2() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("");

    // Act and Assert
    assertTrue(jakartaRegexpMatcher.matches("Argument"));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String)}
   */
  @Test
  public void testMatches3() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act and Assert
    assertTrue(jakartaRegexpMatcher.matches("42"));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches4() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaRegexpMatcher.matches("Input", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches5() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("");

    // Act and Assert
    assertTrue(jakartaRegexpMatcher.matches("Input", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches6() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act and Assert
    assertFalse(jakartaRegexpMatcher.matches("Input", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches7() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaRegexpMatcher.matches("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches8() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaRegexpMatcher.matches("Input", RegexpMatcher.MATCH_MULTILINE));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches9() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaRegexpMatcher.matches("Input", RegexpMatcher.MATCH_SINGLELINE));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches10() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act and Assert
    assertTrue(jakartaRegexpMatcher.matches("42", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches11() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act and Assert
    assertFalse(jakartaRegexpMatcher.matches("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Method under test: {@link JakartaRegexpMatcher#matches(String, int)}
   */
  @Test
  public void testMatches12() throws BuildException {
    // Arrange
    JakartaRegexpMatcher jakartaRegexpMatcher = new JakartaRegexpMatcher();
    jakartaRegexpMatcher.setPattern("42");

    // Act and Assert
    assertTrue(jakartaRegexpMatcher.matches("42", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }
}

