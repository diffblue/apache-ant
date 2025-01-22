package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertThrows;
import javax.xml.transform.TransformerException;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.XSLTProcess;
import org.junit.Test;

public class TraXLiaisonDiffblueTest {
  /**
   * Test {@link TraXLiaison#fatalError(TransformerException)}.
   * <ul>
   *   <li>Given {@link TraXLiaison#TraXLiaison()} Logger is {@link XSLTProcess} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TraXLiaison#fatalError(TransformerException)}
   */
  @Test
  public void testFatalError_givenTraXLiaisonLoggerIsXSLTProcess_thenThrowBuildException() throws Exception {
    // Arrange
    TraXLiaison traXLiaison = new TraXLiaison();
    traXLiaison.setLogger(new XSLTProcess());

    // Act and Assert
    assertThrows(BuildException.class, () -> traXLiaison.fatalError(new TransformerException("foo")));
  }

  /**
   * Test {@link TraXLiaison#fatalError(TransformerException)}.
   * <ul>
   *   <li>Given {@link TraXLiaison#TraXLiaison()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TraXLiaison#fatalError(TransformerException)}
   */
  @Test
  public void testFatalError_givenTraXLiaison_thenThrowBuildException() throws Exception {
    // Arrange
    TraXLiaison traXLiaison = new TraXLiaison();

    // Act and Assert
    assertThrows(BuildException.class, () -> traXLiaison.fatalError(new TransformerException("foo")));
  }
}
