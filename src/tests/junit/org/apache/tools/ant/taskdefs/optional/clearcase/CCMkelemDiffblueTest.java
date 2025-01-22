package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCMkelemDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCMkelem#setCheckin(boolean)}
   *   <li>{@link CCMkelem#setComment(String)}
   *   <li>{@link CCMkelem#setCommentFile(String)}
   *   <li>{@link CCMkelem#setEltype(String)}
   *   <li>{@link CCMkelem#setMaster(boolean)}
   *   <li>{@link CCMkelem#setNoCheckout(boolean)}
   *   <li>{@link CCMkelem#setNoWarn(boolean)}
   *   <li>{@link CCMkelem#setPreserveTime(boolean)}
   *   <li>{@link CCMkelem#getCheckin()}
   *   <li>{@link CCMkelem#getComment()}
   *   <li>{@link CCMkelem#getCommentFile()}
   *   <li>{@link CCMkelem#getEltype()}
   *   <li>{@link CCMkelem#getMaster()}
   *   <li>{@link CCMkelem#getNoCheckout()}
   *   <li>{@link CCMkelem#getNoWarn()}
   *   <li>{@link CCMkelem#getPreserveTime()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCMkelem ccMkelem = new CCMkelem();

    // Act
    ccMkelem.setCheckin(true);
    ccMkelem.setComment("Comment");
    ccMkelem.setCommentFile("Cfile");
    ccMkelem.setEltype("Eltype");
    ccMkelem.setMaster(true);
    ccMkelem.setNoCheckout(true);
    ccMkelem.setNoWarn(true);
    ccMkelem.setPreserveTime(true);
    boolean actualCheckin = ccMkelem.getCheckin();
    String actualComment = ccMkelem.getComment();
    String actualCommentFile = ccMkelem.getCommentFile();
    String actualEltype = ccMkelem.getEltype();
    boolean actualMaster = ccMkelem.getMaster();
    boolean actualNoCheckout = ccMkelem.getNoCheckout();
    boolean actualNoWarn = ccMkelem.getNoWarn();

    // Assert
    assertEquals("Cfile", actualCommentFile);
    assertEquals("Comment", actualComment);
    assertEquals("Eltype", actualEltype);
    assertTrue(actualCheckin);
    assertTrue(actualMaster);
    assertTrue(actualNoCheckout);
    assertTrue(actualNoWarn);
    assertTrue(ccMkelem.getPreserveTime());
  }

  /**
   * Test new {@link CCMkelem} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMkelem}
   */
  @Test
  public void testNewCCMkelem() {
    // Arrange and Act
    CCMkelem actualCcMkelem = new CCMkelem();

    // Assert
    assertEquals("cleartool", actualCcMkelem.getClearToolCommand());
    assertNull(actualCcMkelem.getDescription());
    assertNull(actualCcMkelem.getTaskName());
    assertNull(actualCcMkelem.getTaskType());
    assertNull(actualCcMkelem.getComment());
    assertNull(actualCcMkelem.getCommentFile());
    assertNull(actualCcMkelem.getEltype());
    assertNull(actualCcMkelem.getObjSelect());
    assertNull(actualCcMkelem.getViewPath());
    assertNull(actualCcMkelem.getProject());
    assertNull(actualCcMkelem.getOwningTarget());
    assertFalse(actualCcMkelem.getCheckin());
    assertFalse(actualCcMkelem.getMaster());
    assertFalse(actualCcMkelem.getNoCheckout());
    assertFalse(actualCcMkelem.getNoWarn());
    assertFalse(actualCcMkelem.getPreserveTime());
    assertTrue(actualCcMkelem.getFailOnErr());
  }
}
