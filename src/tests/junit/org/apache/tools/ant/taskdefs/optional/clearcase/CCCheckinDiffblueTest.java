package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCCheckinDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCCheckin#setComment(String)}
   *   <li>{@link CCCheckin#setCommentFile(String)}
   *   <li>{@link CCCheckin#setIdentical(boolean)}
   *   <li>{@link CCCheckin#setKeepCopy(boolean)}
   *   <li>{@link CCCheckin#setNoWarn(boolean)}
   *   <li>{@link CCCheckin#setPreserveTime(boolean)}
   *   <li>{@link CCCheckin#getComment()}
   *   <li>{@link CCCheckin#getCommentFile()}
   *   <li>{@link CCCheckin#getIdentical()}
   *   <li>{@link CCCheckin#getKeepCopy()}
   *   <li>{@link CCCheckin#getNoWarn()}
   *   <li>{@link CCCheckin#getPreserveTime()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCCheckin ccCheckin = new CCCheckin();

    // Act
    ccCheckin.setComment("Comment");
    ccCheckin.setCommentFile("Cfile");
    ccCheckin.setIdentical(true);
    ccCheckin.setKeepCopy(true);
    ccCheckin.setNoWarn(true);
    ccCheckin.setPreserveTime(true);
    String actualComment = ccCheckin.getComment();
    String actualCommentFile = ccCheckin.getCommentFile();
    boolean actualIdentical = ccCheckin.getIdentical();
    boolean actualKeepCopy = ccCheckin.getKeepCopy();
    boolean actualNoWarn = ccCheckin.getNoWarn();

    // Assert
    assertEquals("Cfile", actualCommentFile);
    assertEquals("Comment", actualComment);
    assertTrue(actualIdentical);
    assertTrue(actualKeepCopy);
    assertTrue(actualNoWarn);
    assertTrue(ccCheckin.getPreserveTime());
  }

  /**
   * Test new {@link CCCheckin} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCCheckin}
   */
  @Test
  public void testNewCCCheckin() {
    // Arrange and Act
    CCCheckin actualCcCheckin = new CCCheckin();

    // Assert
    assertEquals("cleartool", actualCcCheckin.getClearToolCommand());
    assertNull(actualCcCheckin.getDescription());
    assertNull(actualCcCheckin.getTaskName());
    assertNull(actualCcCheckin.getTaskType());
    assertNull(actualCcCheckin.getComment());
    assertNull(actualCcCheckin.getCommentFile());
    assertNull(actualCcCheckin.getObjSelect());
    assertNull(actualCcCheckin.getViewPath());
    assertNull(actualCcCheckin.getProject());
    assertNull(actualCcCheckin.getOwningTarget());
    assertFalse(actualCcCheckin.getKeepCopy());
    assertFalse(actualCcCheckin.getNoWarn());
    assertFalse(actualCcCheckin.getPreserveTime());
    assertTrue(actualCcCheckin.getIdentical());
    assertTrue(actualCcCheckin.getFailOnErr());
  }
}
