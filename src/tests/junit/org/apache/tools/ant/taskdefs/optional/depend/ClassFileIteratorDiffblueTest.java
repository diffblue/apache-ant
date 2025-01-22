package org.apache.tools.ant.taskdefs.optional.depend;

import static org.junit.Assert.assertFalse;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import org.junit.Test;

public class ClassFileIteratorDiffblueTest {
  /**
   * Test {@link ClassFileIterator#iterator()}.
   * <ul>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassFileIterator#iterator()}
   */
  @Test
  public void testIterator_thenReturnNotHasNext() throws IOException {
    // Arrange, Act and Assert
    assertFalse((new JarFileIterator(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")))).iterator().hasNext());
  }
}
