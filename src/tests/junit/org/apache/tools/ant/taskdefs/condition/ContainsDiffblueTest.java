package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ContainsDiffblueTest {
  /**
   * Test {@link Contains#eval()}.
   * <ul>
   *   <li>Given {@link Contains} (default constructor) Casesensitive is {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Contains#eval()}
   */
  @Test
  public void testEval_givenContainsCasesensitiveIsTrue_thenReturnFalse() throws BuildException {
    // Arrange
    Contains contains = new Contains();
    contains.setString("foo");
    contains.setSubstring("both string and substring are required in contains");
    contains.setCasesensitive(true);

    // Act and Assert
    assertFalse(contains.eval());
  }

  /**
   * Test {@link Contains#eval()}.
   * <ul>
   *   <li>Given {@link Contains} (default constructor) Substring is {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Contains#eval()}
   */
  @Test
  public void testEval_givenContainsSubstringIsFoo_thenReturnTrue() throws BuildException {
    // Arrange
    Contains contains = new Contains();
    contains.setString("foo");
    contains.setSubstring("foo");
    contains.setCasesensitive(false);

    // Act and Assert
    assertTrue(contains.eval());
  }

  /**
   * Test {@link Contains#eval()}.
   * <ul>
   *   <li>Given {@link Contains} (default constructor) Substring is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Contains#eval()}
   */
  @Test
  public void testEval_givenContainsSubstringIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    Contains contains = new Contains();
    contains.setString("foo");
    contains.setSubstring(null);
    contains.setCasesensitive(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> contains.eval());
  }

  /**
   * Test {@link Contains#eval()}.
   * <ul>
   *   <li>Given {@link Contains} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Contains#eval()}
   */
  @Test
  public void testEval_givenContains_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Contains()).eval());
  }

  /**
   * Test {@link Contains#eval()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Contains#eval()}
   */
  @Test
  public void testEval_thenReturnFalse() throws BuildException {
    // Arrange
    Contains contains = new Contains();
    contains.setString("foo");
    contains.setSubstring("both string and substring are required in contains");
    contains.setCasesensitive(false);

    // Act and Assert
    assertFalse(contains.eval());
  }
}
