package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCMkdirDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCMkdir#setComment(String)}
   *   <li>{@link CCMkdir#setCommentFile(String)}
   *   <li>{@link CCMkdir#setNoCheckout(boolean)}
   *   <li>{@link CCMkdir#getComment()}
   *   <li>{@link CCMkdir#getCommentFile()}
   *   <li>{@link CCMkdir#getNoCheckout()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCMkdir ccMkdir = new CCMkdir();

    // Act
    ccMkdir.setComment("Comment");
    ccMkdir.setCommentFile("Cfile");
    ccMkdir.setNoCheckout(true);
    String actualComment = ccMkdir.getComment();
    String actualCommentFile = ccMkdir.getCommentFile();

    // Assert
    assertEquals("Cfile", actualCommentFile);
    assertEquals("Comment", actualComment);
    assertTrue(ccMkdir.getNoCheckout());
  }

  /**
   * Test new {@link CCMkdir} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMkdir}
   */
  @Test
  public void testNewCCMkdir() {
    // Arrange and Act
    CCMkdir actualCcMkdir = new CCMkdir();

    // Assert
    assertEquals("cleartool", actualCcMkdir.getClearToolCommand());
    assertNull(actualCcMkdir.getDescription());
    assertNull(actualCcMkdir.getTaskName());
    assertNull(actualCcMkdir.getTaskType());
    assertNull(actualCcMkdir.getComment());
    assertNull(actualCcMkdir.getCommentFile());
    assertNull(actualCcMkdir.getObjSelect());
    assertNull(actualCcMkdir.getViewPath());
    assertNull(actualCcMkdir.getProject());
    assertNull(actualCcMkdir.getOwningTarget());
    assertFalse(actualCcMkdir.getNoCheckout());
    assertTrue(actualCcMkdir.getFailOnErr());
  }
}
