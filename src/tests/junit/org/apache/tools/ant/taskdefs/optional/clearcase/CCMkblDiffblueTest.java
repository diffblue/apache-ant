package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCMkblDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCMkbl#setBaselineRootName(String)}
   *   <li>{@link CCMkbl#setComment(String)}
   *   <li>{@link CCMkbl#setCommentFile(String)}
   *   <li>{@link CCMkbl#setFull(boolean)}
   *   <li>{@link CCMkbl#setIdentical(boolean)}
   *   <li>{@link CCMkbl#setNlabel(boolean)}
   *   <li>{@link CCMkbl#setNoWarn(boolean)}
   *   <li>{@link CCMkbl#getBaselineRootName()}
   *   <li>{@link CCMkbl#getComment()}
   *   <li>{@link CCMkbl#getCommentFile()}
   *   <li>{@link CCMkbl#getFull()}
   *   <li>{@link CCMkbl#getIdentical()}
   *   <li>{@link CCMkbl#getNlabel()}
   *   <li>{@link CCMkbl#getNoWarn()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCMkbl ccMkbl = new CCMkbl();

    // Act
    ccMkbl.setBaselineRootName("Baseline Root Name");
    ccMkbl.setComment("Comment");
    ccMkbl.setCommentFile("Cfile");
    ccMkbl.setFull(true);
    ccMkbl.setIdentical(true);
    ccMkbl.setNlabel(true);
    ccMkbl.setNoWarn(true);
    String actualBaselineRootName = ccMkbl.getBaselineRootName();
    String actualComment = ccMkbl.getComment();
    String actualCommentFile = ccMkbl.getCommentFile();
    boolean actualFull = ccMkbl.getFull();
    boolean actualIdentical = ccMkbl.getIdentical();
    boolean actualNlabel = ccMkbl.getNlabel();

    // Assert
    assertEquals("Baseline Root Name", actualBaselineRootName);
    assertEquals("Cfile", actualCommentFile);
    assertEquals("Comment", actualComment);
    assertTrue(actualFull);
    assertTrue(actualIdentical);
    assertTrue(actualNlabel);
    assertTrue(ccMkbl.getNoWarn());
  }

  /**
   * Test new {@link CCMkbl} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMkbl}
   */
  @Test
  public void testNewCCMkbl() {
    // Arrange and Act
    CCMkbl actualCcMkbl = new CCMkbl();

    // Assert
    assertEquals("cleartool", actualCcMkbl.getClearToolCommand());
    assertNull(actualCcMkbl.getDescription());
    assertNull(actualCcMkbl.getTaskName());
    assertNull(actualCcMkbl.getTaskType());
    assertNull(actualCcMkbl.getBaselineRootName());
    assertNull(actualCcMkbl.getComment());
    assertNull(actualCcMkbl.getCommentFile());
    assertNull(actualCcMkbl.getObjSelect());
    assertNull(actualCcMkbl.getViewPath());
    assertNull(actualCcMkbl.getProject());
    assertNull(actualCcMkbl.getOwningTarget());
    assertFalse(actualCcMkbl.getFull());
    assertFalse(actualCcMkbl.getNlabel());
    assertFalse(actualCcMkbl.getNoWarn());
    assertTrue(actualCcMkbl.getIdentical());
    assertTrue(actualCcMkbl.getFailOnErr());
  }
}
