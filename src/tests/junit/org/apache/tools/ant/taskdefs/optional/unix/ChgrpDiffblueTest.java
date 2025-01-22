package org.apache.tools.ant.taskdefs.optional.unix;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ChgrpDiffblueTest {
  /**
   * Test new {@link Chgrp} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Chgrp}
   */
  @Test
  public void testNewChgrp() {
    // Arrange and Act
    Chgrp actualChgrp = new Chgrp();

    // Assert
    assertNull(actualChgrp.getDescription());
    assertNull(actualChgrp.getTaskName());
    assertNull(actualChgrp.getTaskType());
    assertNull(actualChgrp.getOs());
    assertNull(actualChgrp.getOsFamily());
    assertNull(actualChgrp.getProject());
    assertNull(actualChgrp.getOwningTarget());
    assertFalse(actualChgrp.getResolveExecutable());
    assertTrue(actualChgrp.isValidOs());
  }

  /**
   * Test {@link Chgrp#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Chgrp} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chgrp#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenChgrp_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chgrp()).checkConfiguration());
  }

  /**
   * Test {@link Chgrp#setExecutable(String)}.
   * <p>
   * Method under test: {@link Chgrp#setExecutable(String)}
   */
  @Test
  public void testSetExecutable() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chgrp()).setExecutable("foo"));
  }
}
