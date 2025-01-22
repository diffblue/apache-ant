package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class CCMklabelDiffblueTest {
  /**
   * Test {@link CCMklabel#execute()}.
   * <ul>
   *   <li>Given {@link CCMklabel} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMklabel#execute()}
   */
  @Test
  public void testExecute_givenCCMklabel_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CCMklabel()).execute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCMklabel#setComment(String)}
   *   <li>{@link CCMklabel#setCommentFile(String)}
   *   <li>{@link CCMklabel#setRecurse(boolean)}
   *   <li>{@link CCMklabel#setReplace(boolean)}
   *   <li>{@link CCMklabel#setTypeName(String)}
   *   <li>{@link CCMklabel#setVOB(String)}
   *   <li>{@link CCMklabel#setVersion(String)}
   *   <li>{@link CCMklabel#getComment()}
   *   <li>{@link CCMklabel#getCommentFile()}
   *   <li>{@link CCMklabel#getRecurse()}
   *   <li>{@link CCMklabel#getReplace()}
   *   <li>{@link CCMklabel#getTypeName()}
   *   <li>{@link CCMklabel#getVOB()}
   *   <li>{@link CCMklabel#getVersion()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCMklabel ccMklabel = new CCMklabel();

    // Act
    ccMklabel.setComment("Comment");
    ccMklabel.setCommentFile("Cfile");
    ccMklabel.setRecurse(true);
    ccMklabel.setReplace(true);
    ccMklabel.setTypeName("Tn");
    ccMklabel.setVOB("Vob");
    ccMklabel.setVersion("1.0.2");
    String actualComment = ccMklabel.getComment();
    String actualCommentFile = ccMklabel.getCommentFile();
    boolean actualRecurse = ccMklabel.getRecurse();
    boolean actualReplace = ccMklabel.getReplace();
    String actualTypeName = ccMklabel.getTypeName();
    String actualVOB = ccMklabel.getVOB();

    // Assert
    assertEquals("1.0.2", ccMklabel.getVersion());
    assertEquals("Cfile", actualCommentFile);
    assertEquals("Comment", actualComment);
    assertEquals("Tn", actualTypeName);
    assertEquals("Vob", actualVOB);
    assertTrue(actualRecurse);
    assertTrue(actualReplace);
  }

  /**
   * Test new {@link CCMklabel} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMklabel}
   */
  @Test
  public void testNewCCMklabel() {
    // Arrange and Act
    CCMklabel actualCcMklabel = new CCMklabel();

    // Assert
    assertEquals("cleartool", actualCcMklabel.getClearToolCommand());
    assertNull(actualCcMklabel.getDescription());
    assertNull(actualCcMklabel.getTaskName());
    assertNull(actualCcMklabel.getTaskType());
    assertNull(actualCcMklabel.getComment());
    assertNull(actualCcMklabel.getCommentFile());
    assertNull(actualCcMklabel.getTypeName());
    assertNull(actualCcMklabel.getVOB());
    assertNull(actualCcMklabel.getVersion());
    assertNull(actualCcMklabel.getObjSelect());
    assertNull(actualCcMklabel.getViewPath());
    assertNull(actualCcMklabel.getProject());
    assertNull(actualCcMklabel.getOwningTarget());
    assertFalse(actualCcMklabel.getRecurse());
    assertFalse(actualCcMklabel.getReplace());
    assertTrue(actualCcMklabel.getFailOnErr());
  }
}
